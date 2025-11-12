import scala.util.Random

object Renderer {

  // Recursive path tracer that evaluates radiance carried by a given ray.
  def rayColor(ray: Ray, background: Color, world: Hittable, depth: Int): Color = {
    // Stop bouncing once we hit the recursion budget.
    if (depth <= 0) return Color(0, 0, 0)

    world.hit(ray, 0.001, Double.PositiveInfinity) match {
      case Some(rec) =>
        rec.material match {
          case Lambertian(albedo) =>
            // Diffuse surfaces scatter in a random hemisphere while avoiding degenerate vectors.
            val scatterDirection = rec.normal + Vec3.randomUnitVector()
            val finalScatterDir = if (scatterDirection.nearZero) rec.normal else scatterDirection
            val scattered = Ray(rec.p, finalScatterDir)
            albedo * rayColor(scattered, background, world, depth - 1)
          case Metal(albedo, fuzz) =>
            // Metals reflect with an optional fuzz cone and only contribute if the ray exits the surface.
            val reflected = ray.direction.unit.reflect(rec.normal)
            val scattered = Ray(rec.p, reflected + Vec3.randomInUnitSphere() * fuzz)
            if (scattered.direction.dot(rec.normal) > 0) {
              albedo * rayColor(scattered, background, world, depth - 1)
            } else {
              Color(0, 0, 0)
            }
          case Dielectric(ir) =>
            // Glass balances refraction and reflection using Schlick's approximation.
            val refractionRatio = if (rec.frontFace) (1.0 / ir) else ir
            val unitDirection = ray.direction.unit
            val cosTheta = math.min(-unitDirection.dot(rec.normal), 1.0)
            val sinTheta = math.sqrt(1.0 - cosTheta * cosTheta)

            val cannotRefract = refractionRatio * sinTheta > 1.0
            val direction =
              if (cannotRefract || reflectance(cosTheta, refractionRatio) > Random.nextDouble())
                unitDirection.reflect(rec.normal)
              else
                unitDirection.refract(rec.normal, refractionRatio)
            
            val scattered = Ray(rec.p, direction)
            rayColor(scattered, background, world, depth - 1)
        }
      case None =>
        background
    }
  }

  private def reflectance(cosine: Double, refIdx: Double): Double = {
    // Use Schlick's approximation for reflectance.
    var r0 = (1 - refIdx) / (1 + refIdx)
    r0 = r0 * r0
    r0 + (1 - r0) * math.pow((1 - cosine), 5)
  }


  def render(
      imageWidth: Int,
      imageHeight: Int,
      world: HittableList,
      cam: Camera,
      background: Color,
      maxDepth: Int,
      samplesPerPixel: Int
  ): Array[Array[Color]] = {
    // Render scanlines from top to bottom so the stored image matches screen orientation.
    val image = Array.ofDim[Color](imageHeight, imageWidth)

    for (j <- (imageHeight - 1) to 0 by -1) {
      println(s"\rScanlines remaining: $j ")
      for (i <- 0 until imageWidth) {
        var pixelColor = Color(0, 0, 0)
        // Jitter sample positions within the pixel to perform Monte Carlo anti-aliasing.
        for (s <- 0 until samplesPerPixel) {
          val u = (i + Random.nextDouble()) / (imageWidth - 1)
          val v = (j + Random.nextDouble()) / (imageHeight - 1)
          val r = cam.getRay(u, v)
          pixelColor += rayColor(r, background, world, maxDepth)
        }
        image(j)(i) = pixelColor / samplesPerPixel
      }
    }
    println("\nDone.")
    image
  }
}

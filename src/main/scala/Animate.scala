import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

object Animate {

  def main(args: Array[String]): Unit = {
    // Animation parameters that define total duration and output location.
    val numFrames = 240 // How many frames to generate. 120 frames at 24fps = 5 second video.
    val outputDirectory = "frames"

    // Image parameters tuned to keep animation renders manageable.
    val aspectRatio = 16.0 / 9.0
    val imageWidth = 800 // Reduced width for faster animation rendering
    val imageHeight = (imageWidth / aspectRatio).toInt
    val samplesPerPixel = 100 // Reduced for faster rendering
    val maxDepth = 50

    // Scene definition reused across frames, matching scene_complex.txt layout.
    val background = Color(0.70, 0.80, 1.00)
    val cam = Camera(
      lookFrom = Vec3(13, 2, 3),
      lookAt = Vec3(0, 0, 0),
      vup = Vec3(0, 1, 0),
      vfov = 20.0,
      aspectRatio = aspectRatio,
      aperture = 0.1,
      focusDist = 10.0
    )

    // Ensure the frame directory exists so encoders can consume the sequence later.
    new File(outputDirectory).mkdirs()

    println(s"Generating $numFrames frames...")

    // Main animation loop that re-renders the scene with an updated moving object.
    (0 until numFrames).foreach { frame =>
      val startTime = System.currentTimeMillis()

      // Define the moving sphere path so it glides from left to right.
      val t = frame.toDouble / (numFrames - 1) // Progress from 0.0 to 1.0
      val startPos = Vec3(-10, 0.3, 1.8)
      val endPos = Vec3(10, 0.3, 1.8)
      val currentPos = startPos + (endPos - startPos) * t
      val movingSphere = Sphere(currentPos, 0.3, Metal(Color(0.8, 0.3, 0.3), 0.05))


      // Positions follow the camera convention of front-up-left axes.

      // Static geometry reused across every frame.
      val staticWorld: List[Hittable] = List(
        // Ground plane approximated by a huge sphere.
        Sphere(Vec3(0, -1000, 0), 1000, Lambertian(Color(0.5, 0.5, 0.5))),
        // Hero spheres that anchor the composition.
        Sphere(Vec3(0, 1, -0), 1.0, Dielectric(1.5)),
        Sphere(Vec3(-4, 1, -0), 1.0, Lambertian(Color(0.4, 0.2, 0.1))),
        Sphere(Vec3(4, 1, -0), 1.0, Metal(Color(0.7, 0.6, 0.5), 0.0)),
        // Accent spheres borrowed from the static scene definition.
        Sphere(Vec3(5.5, 0.2, 0.0), 0.2, Metal(Color(0.8, 0.8, 0.8), 0.1)),
        Sphere(Vec3(3.5, 0.2, 2.8), 0.2, Metal(Color(0.6, 0.8, 0.9), 0.05)),
        Sphere(Vec3(-1.0, 0.2, 2.5), 0.2, Lambertian(Color(0.2, 0.8, 0.2))),
        Sphere(Vec3(0.8, 0.2, 0.5), 0.2, Lambertian(Color(0.7, 0.3, 0.5))),
        Sphere(Vec3(4.0, 0.2, 1.0), 0.2, Dielectric(1.5))
      )

      // Combine static objects with the moving sphere to get the per-frame world.
      val worldForFrame = HittableList(movingSphere :: staticWorld)

      // Render the frame by sampling every pixel with stratified jitter.
      val colorMatrix = Array.ofDim[Color](imageHeight, imageWidth)
      (0 until imageHeight).foreach { j =>
        val row = (0 until imageWidth).map { i =>
          var pixelColor = Color(0, 0, 0)
          for (s <- 0 until samplesPerPixel) {
            val u = (i + scala.util.Random.nextDouble()) / (imageWidth - 1)
            val v = (j + scala.util.Random.nextDouble()) / (imageHeight - 1)
            val r = cam.getRay(u, v)
            pixelColor += Renderer.rayColor(r, background, worldForFrame, maxDepth)
          }
          pixelColor / samplesPerPixel
        }.toArray
        // Invert the row index so the saved image is upright.
        colorMatrix(imageHeight - 1 - j) = row
      }

      // Persist the sampled frame to disk so ffmpeg can assemble a video later.
      val outputImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
      for (y <- 0 until imageHeight) {
        for (x <- 0 until imageWidth) {
          outputImage.setRGB(x, y, colorMatrix(y)(x).toInt)
        }
      }

      val frameFile = new File(f"$outputDirectory/frame_$frame%04d.png")
      ImageIO.write(outputImage, "png", frameFile)
      
      val endTime = System.currentTimeMillis()
      val duration = (endTime - startTime) / 1000.0
      // Synchronize logging to stay readable if rendering becomes parallelized.
      synchronized {
        println(f"Frame $frame%04d completed in $duration%.2f seconds. Saved to ${frameFile.getPath}")
      }
    }
    println("\nAll frames generated successfully!")
  }
}

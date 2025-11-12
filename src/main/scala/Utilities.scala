import scala.util.Random

// Utility helpers shared across rendering components.
object Utils {
  def clamp(x: Double, min: Double, max: Double): Double = {
    if (x < min) min
    else if (x > max) max
    else x
  }
}

// Core math types and helpers used throughout the ray tracer.
case class Vec3(x: Double, y: Double, z: Double) {
  def unary_- : Vec3 = Vec3(-x, -y, -z)
  def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)
  def -(other: Vec3): Vec3 = Vec3(x - other.x, y - other.y, z - other.z)
  def *(scalar: Double): Vec3 = Vec3(x * scalar, y * scalar, z * scalar)
  def /(scalar: Double): Vec3 = Vec3(x / scalar, y / scalar, z / scalar)
  def dot(other: Vec3): Double = x * other.x + y * other.y + z * other.z
  def cross(other: Vec3): Vec3 = Vec3(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x
  )
  def lengthSquared: Double = x * x + y * y + z * z
  def length: Double = math.sqrt(lengthSquared)
  def unit: Vec3 = this / length
  def nearZero: Boolean = {
    val s = 1e-8
    math.abs(x) < s && math.abs(y) < s && math.abs(z) < s
  }
  def reflect(n: Vec3): Vec3 = this - n * 2 * this.dot(n)
  def refract(n: Vec3, etaiOverEtat: Double): Vec3 = {
    val cosTheta = math.min((-this).dot(n), 1.0)
    val rOutPerp = (this + n * cosTheta) * etaiOverEtat
    val rOutParallel = n * -math.sqrt(math.abs(1.0 - rOutPerp.lengthSquared))
    rOutPerp + rOutParallel
  }
}

// Sampling helpers for generating random vectors in different domains.
object Vec3 {
  def random(): Vec3 = Vec3(Random.nextDouble(), Random.nextDouble(), Random.nextDouble())
  def random(min: Double, max: Double): Vec3 = Vec3(
    Random.nextDouble() * (max - min) + min,
    Random.nextDouble() * (max - min) + min,
    Random.nextDouble() * (max - min) + min
  )
  def randomInUnitSphere(): Vec3 = {
    while (true) {
      val p = Vec3.random(-1, 1)
      if (p.lengthSquared < 1) return p
    }
    Vec3(0,0,0) // Should not happen
  }
  def randomUnitVector(): Vec3 = randomInUnitSphere().unit
  def randomInHemisphere(normal: Vec3): Vec3 = {
    val inUnitSphere = randomInUnitSphere()
    if (inUnitSphere.dot(normal) > 0.0) inUnitSphere
    else -inUnitSphere
  }
   def randomInUnitDisk(): Vec3 = {
    while (true) {
      val p = Vec3(Random.nextDouble() * 2 - 1, Random.nextDouble() * 2 - 1, 0)
      if (p.lengthSquared < 1) return p
    }
    Vec3(0,0,0) // Should not happen
  }
}

// Linear RGB color with gamma-aware conversion into packed integers.
case class Color(r: Double, g: Double, b: Double) {
  def +(other: Color): Color = Color(r + other.r, g + other.g, b + other.b)
  def *(scalar: Double): Color = Color(r * scalar, g * scalar, b * scalar)
  def *(other: Color): Color = Color(r * other.r, g * other.g, b * other.b)
  def /(scalar: Double): Color = Color(r / scalar, g / scalar, b / scalar)
  def toInt: Int = {
    // Apply gamma correction (sqrt), clamp, and scale to [0, 255]
    val ir = (256 * Utils.clamp(math.sqrt(r), 0.0, 0.999)).toInt
    val ig = (256 * Utils.clamp(math.sqrt(g), 0.0, 0.999)).toInt
    val ib = (256 * Utils.clamp(math.sqrt(b), 0.0, 0.999)).toInt
    // Combine into an ARGB integer (with full opacity)
    (255 << 24) | (ir << 16) | (ig << 8) | ib
  }
}

// Ray represented by an origin and direction.
case class Ray(origin: Vec3, direction: Vec3) {
  def at(t: Double): Vec3 = origin + direction * t
}

// Material definitions describing how surfaces scatter light.
sealed trait Material
case class Lambertian(albedo: Color) extends Material
case class Metal(albedo: Color, fuzz: Double) extends Material
case class Dielectric(indexOfRefraction: Double) extends Material

// Geometry primitives that rays can intersect.
case class HitRecord(
    p: Vec3,
    normal: Vec3,
    material: Material,
    t: Double,
    frontFace: Boolean
) {
  def setFaceNormal(ray: Ray, outwardNormal: Vec3): HitRecord = {
    val front = ray.direction.dot(outwardNormal) < 0
    val n = if (front) outwardNormal else -outwardNormal
    this.copy(frontFace = front, normal = n)
  }
}

sealed trait Hittable {
    def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord]
}

case class Sphere(center: Vec3, radius: Double, material: Material) extends Hittable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val oc = ray.origin - center
    val a = ray.direction.lengthSquared
    val halfB = oc.dot(ray.direction)
    val c = oc.lengthSquared - radius * radius
    val discriminant = halfB * halfB - a * c

    if (discriminant < 0) return None

    val sqrtd = math.sqrt(discriminant)
    var root = (-halfB - sqrtd) / a
    if (root < tMin || tMax < root) {
      root = (-halfB + sqrtd) / a
      if (root < tMin || tMax < root) return None
    }

    val t = root
    val p = ray.at(t)
    val outwardNormal = (p - center) / radius
    val rec = HitRecord(p, Vec3(0,0,0), material, t, false)
    Some(rec.setFaceNormal(ray, outwardNormal))
  }
}

// Aggregates primitives and returns the closest hit along a ray.
case class HittableList(objects: List[Hittable]) extends Hittable {
    def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
        objects.foldLeft(None: Option[HitRecord]) { (closestHit, currentObj) =>
            val newTMax = closestHit.map(_.t).getOrElse(tMax)
            currentObj.hit(ray, tMin, newTMax) match {
                case Some(newHit) => Some(newHit)
                case None => closestHit
            }
        }
    }
}


// Thin-lens camera that produces depth-of-field effects.
case class Camera(
    lookFrom: Vec3,
    lookAt: Vec3,
    vup: Vec3,
    vfov: Double, // vertical field-of-view in degrees
    aspectRatio: Double,
    aperture: Double,
    focusDist: Double
) {
  private val theta = math.toRadians(vfov)
  private val h = math.tan(theta / 2)
  private val viewportHeight = 2.0 * h
  private val viewportWidth = aspectRatio * viewportHeight

  private val w = (lookFrom - lookAt).unit
  private val u = vup.cross(w).unit
  private val v = w.cross(u)

  val origin: Vec3 = lookFrom
  val horizontal: Vec3 = u * viewportWidth * focusDist
  val vertical: Vec3 = v * viewportHeight * focusDist
  val lowerLeftCorner: Vec3 = origin - horizontal / 2 - vertical / 2 - w * focusDist
  val lensRadius: Double = aperture / 2

  def getRay(s: Double, t: Double): Ray = {
    val rd = Vec3.randomInUnitDisk() * lensRadius
    val offset = u * rd.x + v * rd.y
    Ray(
      origin + offset,
      lowerLeftCorner + horizontal * s + vertical * t - origin - offset
    )
  }
}

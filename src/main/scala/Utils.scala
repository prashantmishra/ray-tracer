import scalafx.scene.paint.Color

case class View(size: List[Int], distance: Int, objects: List[Model], lights: List[Light])

case class Ray(position: Vector, direction: Vector) {
  def *(v: Double): Vector = position + direction * v
}

case class Vector(x: Double, y: Double, z: Double) {
  def op(f: (Double, Double) => Double)(v: Vector): Vector = Vector(f(x, v.x), f(y, v.y), f(z, v.z))
  def +(v: Vector): Vector = op(_ + _)(v)
  def -(v: Vector): Vector = op(_ - _)(v)
  def *(v: Vector): Vector = op(_ * _)(v)
  def /(v: Vector): Vector = op(_ / _)(v)
  def *(v: Double): Vector = Vector(x * v, y * v, z * v)
  def /(v: Double): Vector = Vector(x / v, y / v, z / v)
  def dot(v: Vector): Double = x * v.x + y * v.y + z * v.z
  def *+(v: Vector): Double = {
    val t = this * v
    t.x + t.y + t.z
  }
  def lengthSquared: Double = this *+ this
  def length: Double = Math.sqrt(lengthSquared)
  def norm: Vector = this / length
}

abstract class Model {
  def position: Vector
  def color: Color
  def ambient: Float
  def diffuse: Float
  def speculing: Float
  def shininess: Int
  def refractiveIndex: Float
  def intersectRay(ray: Ray): Double
  def reflectedVector(point: Vector, ray: Ray): Vector
  def refractedVector(point: Vector, ray: Ray): Vector
  def reflectance(point: Vector, ray: Ray): Float
  def normal(point: Vector): Vector
  def getColor(point: Vector): Color
}

case class Sphere(
    radius: Double,
    position: Vector,
    color: Color,
    ambient: Float,
    diffuse: Float,
    speculing: Float,
    shininess: Int,
    refractiveIndex: Float
) extends Model {

  override def intersectRay(ray: Ray): Double = {
    val v = position - ray.position
    val a = v *+ ray.direction
    val b = v.lengthSquared - (radius * radius)
    val c = a * a - b
    if (c >= 0) a - Math.sqrt(c) else -1
  }

  override def reflectedVector(normal: Vector, ray: Ray): Vector = {
    ray.direction.norm - normal.norm * (2 * normal.norm.dot(ray.direction.norm))
  }

  override def refractedVector(point: Vector, incidentRay: Ray): Vector = {
    var normal: Vector = point - this.position
    var cos_i: Double = incidentRay.direction.dot(normal) / (normal.length * incidentRay.direction.length)
    val relativeIndex: Double = if (cos_i > 0) refractiveIndex else 1 / refractiveIndex

    normal = if (cos_i > 0) normal.norm * -1 else normal.norm
    cos_i = Math.abs(cos_i)
    val cos_t_sq = 1 - (relativeIndex * relativeIndex * (1 - cos_i * cos_i))

    if (cos_t_sq < 0) {
      reflectedVector(normal, incidentRay)
    } else {
      incidentRay.direction.norm * relativeIndex + normal * (relativeIndex * cos_i - Math.sqrt(cos_t_sq))
    }
  }

  override def reflectance(point: Vector, incidentRay: Ray): Float = {
    if (refractiveIndex < 1) return 1.0f

    var normal: Vector = point - this.position
    var cos_i: Double = incidentRay.direction.dot(normal) / (normal.length * incidentRay.direction.length)
    val relativeIndex: Double = if (cos_i > 0) refractiveIndex else 1 / refractiveIndex

    normal = if (cos_i > 0) normal.norm * -1 else normal.norm
    cos_i = Math.abs(cos_i)
    val cos_t_sq = 1 - (relativeIndex * relativeIndex * (1 - cos_i * cos_i))

    if (cos_t_sq < 0) return 1.0f

    val cos_t = Math.sqrt(cos_t_sq)
    val rPerp = (relativeIndex * cos_i - cos_t) / (relativeIndex * cos_i + cos_t)
    val rPara = (cos_i - relativeIndex * cos_t) / (cos_i + relativeIndex * cos_t)

    ((rPara * rPara + rPerp * rPerp) / 2).toFloat
  }

  override def normal(point: Vector): Vector = point - position
  override def getColor(point: Vector): Color = this.color
}

case class Plane(
    position: Vector,
    color: Color,
    ambient: Float,
    diffuse: Float,
    speculing: Float,
    shininess: Int
) extends Model {

  override def refractiveIndex: Float = -1

  override def intersectRay(ray: Ray): Double = {
    val denominator = position.norm.dot(ray.direction)
    val numerator = (position - ray.position).dot(position.norm)
    if (denominator != 0 && (numerator / denominator) > 0) numerator / denominator else -1
  }

  override def reflectedVector(point: Vector, ray: Ray): Vector = {
    ray.direction.norm - point.norm * (2 * point.norm.dot(ray.direction.norm))
  }

  override def refractedVector(point: Vector, ray: Ray): Vector = null
  override def reflectance(point: Vector, ray: Ray): Float = 1.0f

  override def normal(point: Vector): Vector = {
    val alignment = point.dot(position)
    if (alignment > 0) position.norm * -1 else position.norm
  }

  override def getColor(point: Vector): Color = {
    val l = 180
    var x_diff = (point.x - this.position.x + 1)
    var y_diff = (point.y - this.position.y + 1)
    var z_diff = (point.z - this.position.z + 1)

    x_diff = (((x_diff % l) + l) % l).toInt / (l / 2)
    y_diff = (((y_diff % l) + l) % l).toInt / (l / 2)
    z_diff = (((z_diff % l) + l) % l).toInt / (l / 2)

    var total = x_diff + y_diff + z_diff
    total = ((total % 2) + 2) % 2

    if (total == 0) {
      Color.color(1.0 - this.color.red, 1.0 - this.color.green, 1.0 - this.color.blue)
    } else {
      this.color
    }
  }
}

case class Light(position: Vector, intensity: Float)

object Utilities {

  def addColors(color1: Color, color2: Color): Color = {
    if (color1 == null) return color2
    if (color2 == null) return color1

    Color.color(
      math.min(1.0, color1.red + color2.red),
      math.min(1.0, color1.green + color2.green),
      math.min(1.0, color1.blue + color2.blue)
    )
  }

  def enhanceColor(color: Color, factor: Float): Color = {
    if (color == null) return null
    Color.color(
      math.min(1.0, color.red * factor),
      math.min(1.0, color.green * factor),
      math.min(1.0, color.blue * factor)
    )
  }

  def addColorWeighted(color1: Color, weight1: Int, color2: Color, weight2: Int): Color = {
    if (color1 == null) return color2
    if (color2 == null) return color1

    val totalWeight = weight1 + weight2
    val r = (color1.red * weight1 + color2.red * weight2) / totalWeight
    val g = (color1.green * weight1 + color2.green * weight2) / totalWeight
    val b = (color1.blue * weight1 + color2.blue * weight2) / totalWeight
    Color.color(r, g, b)
  }
}
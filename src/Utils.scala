import scala.swing.Color

case class View(size: List[Int], distance: Int, objects: List[Model], lights: List[Light])

case class Ray(position: Vector, direction: Vector) {
  def *(v: Double) = position + direction * v
}

case class Vector(x: Double, y: Double, z: Double) {

  def op(f: (Double, Double) => Double) = (v: Vector) => Vector(f(x, v.x), f(y, v.y), f(z, v.z))

  def + = op(_ + _)

  def - = op(_ - _)

  def * = op(_ * _)

  def / = op(_ / _)

  def *(v: Double) = Vector(x * v, y * v, z * v)

  def /(v: Double) = Vector(x / v, y / v, z / v)

  def dot(v: Vector) = x * v.x + y * v.y + z * v.z

  def *+(v: Vector) = {
    val t = this * v
    t.x + t.y + t.z
  }

  def lengthSquared = this *+ this

  def length = Math.sqrt(lengthSquared)

  def norm = this / length
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

case class Sphere(radius: Double, position: Vector, color: Color, ambient: Float, diffuse: Float, speculing: Float,
                  shininess: Int, refractiveIndex: Float) extends Model {

  override def intersectRay(ray: Ray): Double = {
    val v = position - ray.position
    val a = v *+ ray.direction
    val b = v.lengthSquared - (radius * radius)
    val c = a * a - b
    if (c >= 0) {
      return a - Math.sqrt(c)
    } else {
      return -1
    }
  }

  override def reflectedVector(normal: Vector, ray: Ray): Vector = {
    return ray.direction.norm - normal.norm * (2 * normal.norm.dot(ray.direction.norm))
  }

  override def refractedVector(point: Vector, incidentRay: Ray): Vector = {
    var normal: Vector = point - this.position
    var cos_i = incidentRay.direction.dot(normal) / (normal.length * incidentRay.direction.length)
    val relativeIndex: Double = if (cos_i > 0) refractiveIndex else 1 / refractiveIndex

    normal = if (cos_i > 0) normal.norm * (-1) else normal.norm
    cos_i = Math.abs(cos_i)
    val cos_t_sq = 1 - (relativeIndex * relativeIndex * (1 - cos_i * cos_i))

    if (cos_t_sq < 0) return reflectedVector(normal, incidentRay)
    return incidentRay.direction.norm * relativeIndex + normal * (relativeIndex * cos_i - Math.sqrt(cos_t_sq))
  }

  override def reflectance(point: Vector, incidentRay: Ray): Float = {

    if (refractiveIndex < 1) return 1.0f

    var normal: Vector = point - this.position
    var cos_i = incidentRay.direction.dot(normal) / (normal.length * incidentRay.direction.length)
    val relativeIndex: Double = if (cos_i > 0) refractiveIndex else 1 / refractiveIndex

    normal = if (cos_i > 0) normal.norm * (-1) else normal.norm
    cos_i = Math.abs(cos_i)
    val cos_t_sq = 1 - (relativeIndex * relativeIndex * (1 - cos_i * cos_i))

    if (cos_t_sq < 0) return 1.0f

    val cos_t = Math.sqrt(cos_t_sq)

    val rPerp = (relativeIndex * cos_i - cos_t) / (relativeIndex * cos_i + cos_t)
    val rPara = (cos_i - relativeIndex * cos_t) / (cos_i + relativeIndex * cos_t)

    return ((rPara * rPara + rPerp * rPerp) / 2).toFloat
  }

  override def normal(point: Vector): Vector = {
    return point - position
  }

  override def getColor(point: Vector): Color = {
    this.color
  }
}

case class Plane(position: Vector, color: Color, ambient: Float, diffuse: Float, speculing: Float, shininess: Int)
  extends Model {

  override def refractiveIndex: Float = -1

  override def intersectRay(ray: Ray): Double = {

    val denominator = position.norm.dot(ray.direction)
    val numerator = (position - ray.position).dot(position.norm)

    if (denominator != 0 && (numerator / denominator) > 0) {
      return numerator / denominator
    } else {
      return -1
    }
  }

  override def reflectedVector(point: Vector, ray: Ray): Vector = {
    return ray.direction.norm - point.norm * (2 * point.norm.dot(ray.direction.norm))
  }

  override def refractedVector(point: Vector, ray: Ray): Vector = {
    return null
  }

  override def reflectance(point: Vector, ray: Ray): Float = {
    return 1.0f;
  }

  override def normal(point: Vector): Vector = {
    val alignment = point.dot(position)
    if (alignment > 0) return position.norm * (-1)
    return position.norm
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

    if (total == 0) return new Color(255 - this.color.getRed, 255 - this.color.getGreen, 255 - this.color.getBlue)
    return this.color
  }

}

case class Light(position: Vector, intensity: Float)

object Utilities {

  def addColors(color1: Color, color2: Color): Color = {
    if (color1 == null) return color2
    if (color2 == null) return color1
    var red = (color1.getRed + color2.getRed)
    if (red > 255) red = 255
    var green = (color1.getGreen + color2.getGreen)
    if (green > 255) green = 255
    var blue = (color1.getBlue + color2.getBlue)
    if (blue > 255) blue = 255
    return new Color(red, green, blue)
  }

  def enhanceColor(color: Color, factor: Float): Color = {
    var red = color.getRed * factor
    if (red > 255) red = 255
    var green = color.getGreen * factor
    if (green > 255) green = 255
    var blue = color.getBlue * factor
    if (blue > 255) blue = 255
    return new Color(red.toInt, green.toInt, blue.toInt)
  }

  def addColorWeighted(color1: Color, weight1: Int, color2: Color, weight2: Int): Color = {
    if (color1 == null) return color2
    if (color2 == null) return color1
    val r = (color1.getRed.toDouble * weight1 + color2.getRed.toDouble * weight2) / (weight1 + weight2)
    val g = (color1.getGreen.toDouble * weight1 + color2.getGreen.toDouble * weight2) / (weight1 + weight2)
    val b = (color1.getBlue.toDouble * weight1 + color2.getBlue.toDouble * weight2) / (weight1 + weight2)
    new Color(r.toInt, g.toInt, b.toInt)
  }

}
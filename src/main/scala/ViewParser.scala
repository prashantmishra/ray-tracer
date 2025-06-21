import scala.collection.mutable
import scala.io.Source
import scalafx.scene.paint.Color

class ViewParser {

  def parseView(file: String): View = {
    var distance: Integer = null
    val size = mutable.ListBuffer.empty[Int]
    val objects = mutable.ListBuffer.empty[Model]
    val lights = mutable.ListBuffer.empty[Light]

    for (line <- Source.fromFile(file).getLines()) {
      if (line.nonEmpty && !line.startsWith("#")) {
        val parts = line.split("::", 2)
        if (parts.length == 2) {
          val key = parts(0).trim
          val value = parts(1).trim

          if (key == "Size") {
            val m = mapFromLine(value)
            size += getInt(m, "x")
            size += getInt(m, "y")
          } else if (key == "FOVP") {
            val m = mapFromLine(value)
            distance = getInt(m, "d")
          } else if (key == "Light") {
            val m = mapFromLine(value)
            lights += Light(
              new Vector(getDouble(m, "x"), getDouble(m, "y"), getDouble(m, "z")),
              getFloat(m, "in")
            )
          } else if (key == "Sphere") {
            val m = mapFromLine(value)
            val center = new Vector(getDouble(m, "x"), getDouble(m, "y"), getDouble(m, "z"))
            val color = Color.rgb(getInt(m, "c_r"), getInt(m, "c_g"), getInt(m, "c_b"))
            objects += Sphere(
              getDouble(m, "r"), center, color,
              getFloat(m, "am"), getFloat(m, "di"), getFloat(m, "sp"),
              getInt(m, "sh"), getFloat(m, "ri")
            )
          } else if (key == "Plane") {
            val m = mapFromLine(value)
            val position = new Vector(getDouble(m, "n_x"), getDouble(m, "n_y"), getDouble(m, "n_z"))
            val color = Color.rgb(getInt(m, "c_r"), getInt(m, "c_g"), getInt(m, "c_b"))
            objects += Plane(
              position, color,
              getFloat(m, "am"), getFloat(m, "di"), getFloat(m, "sp"),
              getInt(m, "sh")
            )
          }
        }
      }
    }
    new View(size.toList, distance, objects.toList, lights.toList)
  }

  def mapFromLine(line: String): Map[String, String] = {
    line.trim.split(" ").filter(_.nonEmpty).map { s =>
      val parts = s.split(":")
      (parts(0), parts(1))
    }.toMap
  }

  def getDouble(m: Map[String, String], s: String): Double = m.get(s).get.toDouble
  def getFloat(m: Map[String, String], s: String): Float = m.get(s).get.toFloat
  def getInt(m: Map[String, String], s: String): Int = m.get(s).get.toInt
}
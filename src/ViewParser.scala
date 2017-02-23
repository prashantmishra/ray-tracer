import scala.collection.mutable
import scala.io.Source
import scala.swing.Color

class ViewParser {

  def parseView(file: String): View = {

    var distance: Integer = null
    var size = mutable.ListBuffer.empty[Int]
    var objects = mutable.ListBuffer.empty[Model]
    var lights = mutable.ListBuffer.empty[Light]

    for (line <- Source.fromFile(file).getLines()) {
      if (!line.isEmpty && !line.startsWith("#")) {

        if (line.startsWith("Size")) {

          val m = mapFromLine(line.split("::")(1).trim)
          size += getInt(m, "x")
          size += getInt(m, "y")

        } else if (line.startsWith("FOVP")) {

          val m = mapFromLine(line.split("::")(1).trim)
          distance = getInt(m, "d")

        } else if (line.startsWith("Light")) {

          val m = mapFromLine(line.split("::")(1).trim)
          lights += new Light(new Vector(getDouble(m, "x"), getDouble(m, "y"), getDouble(m, "z")), getFloat(m, "in"))

        } else if (line.startsWith("Sphere")) {

          val m = mapFromLine(line.split("::")(1).trim)
          val center = new Vector(getDouble(m, "x"), getDouble(m, "y"), getDouble(m, "z"))
          val color = new Color(getInt(m, "c_r"), getInt(m, "c_g"), getInt(m, "c_b"))
          objects += new Sphere(getDouble(m, "r"), center, color, getFloat(m, "am"), getFloat(m, "di"),
            getFloat(m, "sp"), getInt(m, "sh"), getFloat(m, "ri"))

        } else if (line.startsWith("Plane")) {

          val m = mapFromLine(line.split("::")(1).trim)
          val position = new Vector(getDouble(m, "n_x"), getDouble(m, "n_y"), getDouble(m, "n_z"))
          val color = new Color(getInt(m, "c_r"), getInt(m, "c_g"), getInt(m, "c_b"))
          objects += new Plane(position, color, getFloat(m, "am"), getFloat(m, "di"), getFloat(m, "sp"),
            getInt(m, "sh"))

        }
      }
    }

    return new View(size.toList, distance, objects.toList, lights.toList)

  }

  def mapFromLine(line: String): Map[String, String] = {
    Map(line.replaceAll(" +", " ").split(" ") map { s => (s.split(":")(0), s.split(":")(1)) }: _*)
  }

  def getDouble(m: Map[String, String], s: String): Double = {
    m.get(s).get.toDouble
  }

  def getFloat(m: Map[String, String], s: String): Float = {
    m.get(s).get.toFloat
  }

  def getInt(m: Map[String, String], s: String): Int = {
    m.get(s).get.toInt
  }

}

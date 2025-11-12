import scala.io.Source
import scala.util.Try

object ViewParser {
  def parseScene(filename: String): (HittableList, Camera, Color, Int, Int) = {
    // Strip comments and whitespace so directives can be parsed reliably.
    val lines = Source.fromFile(filename).getLines().map { line =>
      val commentIndex = line.indexOf('#')
      if (commentIndex != -1) line.substring(0, commentIndex) else line
    }.map(_.trim.toLowerCase).filterNot(_.isEmpty).toList

    // Helper to parse whitespace-delimited triples into vectors.
    def parseVec(s: String): Vec3 = {
        val parts = s.split("\\s+").map(_.toDouble)
        Vec3(parts(0), parts(1), parts(2))
    }
    
    // Helper to parse whitespace-delimited triples into colors.
    def parseColor(s: String): Color = {
        val parts = s.split("\\s+").map(_.toDouble)
        Color(parts(0), parts(1), parts(2))
    }

    // Default scene parameters that can be overridden by the file.
    var world: List[Hittable] = List.empty
    var lookFrom = Vec3(13, 2, 3)
    var lookAt = Vec3(0, 0, 0)
    var vup = Vec3(0, 1, 0)
    var vfov = 40.0
    var aperture = 0.1
    var focusDist = 10.0
    var background = Color(0.7, 0.8, 1.0)
    var maxDepth = 50
    var samplesPerPixel = 100

    // Interpret each directive and mutate the parser state accordingly.
    lines.foreach { line =>
        val parts = line.split("\\s+", 2)
        val key = parts(0)
        val value = if (parts.length > 1) parts(1) else ""
        
        try {
            key match {
                case "background" => background = parseColor(value)
                case "max_depth" => maxDepth = value.toInt
                case "samples_per_pixel" => samplesPerPixel = value.toInt
                case "camera_lookfrom" => lookFrom = parseVec(value)
                case "camera_lookat" => lookAt = parseVec(value)
                case "camera_vup" => vup = parseVec(value)
                case "camera_vfov" => vfov = value.toDouble
                case "camera_aperture" => aperture = value.toDouble
                case "camera_focus_dist" => focusDist = value.toDouble
                case "sphere" =>
                    // Sphere directive layout: x y z radius material params...
                    val sphereData = value.split("\\s+")
                    val center = Vec3(sphereData(0).toDouble, sphereData(1).toDouble, sphereData(2).toDouble)
                    val radius = sphereData(3).toDouble
                    val materialType = sphereData(4)
                    
                    val materialParams = sphereData.drop(5)
                    val material = materialType match {
                        case "lambertian" => Lambertian(Color(materialParams(0).toDouble, materialParams(1).toDouble, materialParams(2).toDouble))
                        case "metal" => Metal(Color(materialParams(0).toDouble, materialParams(1).toDouble, materialParams(2).toDouble), materialParams(3).toDouble)
                        case "dielectric" => Dielectric(materialParams(0).toDouble)
                    }
                    world = Sphere(center, radius, material) :: world // Prepend to the list
                case _ => println(s"Warning: Unknown directive: '$key'")
            }
        } catch {
            case e: Exception => println(s"Warning: Could not parse line '$line'. Error: ${e.getClass.getSimpleName} - ${e.getMessage}")
        }
    }

    // Build the camera and return the hittable list in file order.
    val cam = Camera(lookFrom, lookAt, vup, vfov, 16.0 / 9.0, aperture, focusDist)
    // The world list was built by prepending (::), so we reverse it to match the file order.
    (HittableList(world.reverse), cam, background, maxDepth, samplesPerPixel)
  }
}

import org.slf4j.LoggerFactory
import scalafx.scene.paint.Color
import scala.util.control.Breaks._

class Tracer {

  val depthMax: Int = 5
  val onlyRefraction: Boolean = true
  val logger = LoggerFactory.getLogger(classOf[Tracer])

  def colorMatrix(view: View, aa_samples: Int): Array[Array[Color]] = {
    val eye = new Vector(0, 0, 0)
    val plane_dist = view.distance
    val colors = Array.ofDim[Color](view.size.head, view.size(1))
    val r = scala.util.Random
    val startTime = System.currentTimeMillis()

    for (x <- 0 until view.size.head) {
      logger.info(s"Rendered ${Math.round(x * 100.0 / view.size.head)} percent of the scene..")
      for (y <- 0 until view.size(1)) {
        var color: Color = null
        for (n <- 0 until aa_samples) {
          val vec = new Vector(
            x - view.size.head / 2 + r.nextDouble() - 0.5,
            view.size(1) / 2 - y + r.nextDouble() - 0.5,
            plane_dist
          ).norm
          color = Utilities.addColorWeighted(trace(view, new Ray(eye, vec), 0, returnAmbient = true), 1, color, n)
        }
        colors(x)(y) = color
      }
    }

    logger.info(s"Completed rendering scene in ${(System.currentTimeMillis() - startTime) / 1000} seconds.")
    colors
  }

  def trace(view: View, ray: Ray, depth: Int, returnAmbient: Boolean): Color = {
    if (depth > depthMax) return Color.Black

    var minDistanceToSurface: Double = -1
    var minObject: Model = null

    for (obj <- view.objects) {
      val distanceToSurface = obj.intersectRay(ray)
      if (distanceToSurface > 0.1) {
        if (minDistanceToSurface == -1 || distanceToSurface < minDistanceToSurface) {
          minDistanceToSurface = distanceToSurface
          minObject = obj
        }
      }
    }

    if (minDistanceToSurface > 0) {
      colorCalculate(view, ray, minDistanceToSurface, minObject, depth)
    } else {
      if (returnAmbient) Color.rgb(60, 100, 255) else Color.Black
    }
  }

  def colorCalculate(view: View, traceDir: Ray, distance: Double, obj: Model, depth: Int): Color = {
    val eyeToPoint = traceDir * distance
    val color = colorBasic(view, eyeToPoint, obj)

    val normal = obj.normal(eyeToPoint)
    val lightReflectedVector = obj.reflectedVector(normal, traceDir)
    val lightRefractedVector = obj.refractedVector(eyeToPoint, traceDir)

    var reflectance = obj.reflectance(eyeToPoint, traceDir)
    val refractance = 1 - reflectance
    if (onlyRefraction) reflectance = 0

    val additionalColor = if (obj.refractiveIndex >= 1 && refractance > 0) {
      Utilities.addColorWeighted(
        trace(view, new Ray(eyeToPoint, lightRefractedVector.norm), depth + 1, returnAmbient = true),
        (refractance * 100).toInt,
        Utilities.enhanceColor(trace(view, new Ray(eyeToPoint, lightReflectedVector.norm), depth + 1, returnAmbient = false), 0.4f / (depth + 1)),
        (reflectance * 100).toInt
      )
    } else {
      Utilities.enhanceColor(trace(view, new Ray(eyeToPoint, lightReflectedVector.norm), depth + 1, returnAmbient = true), 0.4f / (depth + 1))
    }

    Utilities.addColors(color, additionalColor)
  }

  def colorBasic(view: View, eyeToPoint: Vector, obj: Model): Color = {
    var red = Utilities.enhanceColor(obj.getColor(eyeToPoint), 0.0f).red
    var green = Utilities.enhanceColor(obj.getColor(eyeToPoint), 0.0f).green
    var blue = Utilities.enhanceColor(obj.getColor(eyeToPoint), 0.0f).blue

    if (obj.refractiveIndex > 1) return null

    for (light <- view.lights) {
      val normal = obj.normal(eyeToPoint)
      val sphereToLight = light.position - obj.position
      val dotProduct = normal.dot(sphereToLight) / (normal.length * sphereToLight.length)

      if (dotProduct >= 0) {
        val surfaceToLight = light.position - eyeToPoint
        val ray = new Ray(eyeToPoint, surfaceToLight.norm)

        breakable {
          for (obj_shadowCheck <- view.objects) {
            if (obj_shadowCheck != obj) {
              val checkIntersect = obj_shadowCheck.intersectRay(ray)
              if (checkIntersect != -1 && checkIntersect > 0 && checkIntersect < surfaceToLight.length && obj_shadowCheck.refractiveIndex < 1) {
                break
              }
            }
          }

          val lightReflectedVector = obj.reflectedVector(normal, ray) * -1
          var phongStrength = -(eyeToPoint.norm.dot(lightReflectedVector.norm))
          if (phongStrength < 0) phongStrength = 0

          val colorAtPoint = obj.getColor(eyeToPoint)
          val lightEffect = light.intensity * (dotProduct + obj.speculing * Math.pow(phongStrength, obj.shininess))

          red += lightEffect * colorAtPoint.red
          green += lightEffect * colorAtPoint.green
          blue += lightEffect * colorAtPoint.blue
        }
      }
    }

    Color.color(
      math.min(red, 1.0),
      math.min(green, 1.0),
      math.min(blue, 1.0)
    )
  }
}
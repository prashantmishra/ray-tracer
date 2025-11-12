import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.image.{ImageView, WritableImage}
import scalafx.scene.layout.StackPane
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

object Main extends JFXApp3 {

  override def start(): Unit = {
    val sceneFilePath = "samples/sample.txt"
    val sceneFile = new File(sceneFilePath)
    if (!sceneFile.exists()) {
      println(s"Error: Scene file not found at '$sceneFilePath'")
      sys.exit(1)
    }

    val aspectRatio = 16.0 / 9.0
    val imageWidth = 1200
    val imageHeight = (imageWidth / aspectRatio).toInt

    // Parse the textual scene description so the renderer knows what to display.
    val (world, cam, background, maxDepth, samplesPerPixel) = ViewParser.parseScene(sceneFilePath)

    // Render the image once so both the UI and on-disk copy stay in sync.
    val colorMatrix = Renderer.render(imageWidth, imageHeight, world, cam, background, maxDepth, samplesPerPixel)

    val writableImage = new WritableImage(imageWidth, imageHeight)
    val pixelWriter = writableImage.pixelWriter

    for (y <- 0 until imageHeight) {
      for (x <- 0 until imageWidth) {
        // The renderer writes scanlines top-down, so flip the UI coordinate when writing pixels.
        // We access colorMatrix(y) but the UI y-coordinate is inverted.
        val uiY = imageHeight - 1 - y
        pixelWriter.setArgb(x, uiY, colorMatrix(y)(x).toInt)
      }
    }
    
    val outputImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
    for (y <- 0 until imageHeight) {
        for (x <- 0 until imageWidth) {
            outputImage.setRGB(x, y, colorMatrix(imageHeight - 1 - y)(x).toInt)
        }
    }
    
    // Persist the rendered PNG next to the scene definition for later reuse.
    val outputFile = new File(sceneFile.getParent, "sample.png")
    ImageIO.write(outputImage, "png", outputFile)
    println(s"Image saved to ${outputFile.getAbsolutePath}")

    stage = new JFXApp3.PrimaryStage {
      title = "Scala Ray Tracer"
      scene = new Scene {
        // Display the rendered frame inside a simple container window.
        root = new StackPane {
          children = new ImageView(writableImage)
        }
      }
    }
  }
}

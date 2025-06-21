import org.slf4j.LoggerFactory
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.Includes._ // <-- ADD THIS LINE
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

object DisplayScene extends JFXApp3 {

  override def start(): Unit = {
    val view = new ViewParser().parseView("samples/sample.txt")
    val colors = new Tracer().colorMatrix(view, 16)
    val logger = LoggerFactory.getLogger(getClass)

    stage = new PrimaryStage {
      title = "Scala Ray Tracer"
      scene = new Scene(view.size.head, view.size(1)) {
        val canvas = new Canvas(view.size.head, view.size(1))
        val g = canvas.graphicsContext2D
        content = canvas

        for (x <- 0 until view.size.head; y <- 0 until view.size(1)) {
          g.pixelWriter.setColor(x, y, colors(x)(y))
        }
      }
      // This line will now work because of the new import
      onCloseRequest = () => {
        logger.info("Exiting...")
        sys.exit(0)
      }
    }
  }
}
import java.awt.Color

import swing.Swing._
import swing.{Color, _}
import event._
import scala.util.control.Breaks._

class MainFrame extends Frame {

  val view: View = (new ViewParser).parseView("objects.txt")
  val colors = (new Tracer).colorMatrix(view, 16)

  title = "Scala Ray Tracer"
  contents = new Panel {

    preferredSize = (view.size(0), view.size(1))
    opaque = true

    override def paint(g: Graphics2D) = {

      background = new Color(0, 0, 0)
      super.paintComponent(g)

      for (x <- 0 until view.size(0)) {
        for (y <- 0 until view.size(1)) {
          g.setColor(colors(x)(y))
          g.drawLine(x, y, x, y)
        }
      }
    }
  }
  centerOnScreen
  listenTo(this)
  reactions += {
    case WindowClosing(e) => {
      println("Exiting...")
      System.exit(0)
    }
  }
}

object DisplayScene extends SimpleSwingApplication {
  def top = new MainFrame
}
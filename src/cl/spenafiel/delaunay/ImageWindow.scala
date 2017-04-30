package cl.spenafiel.delaunay

import javafx.event.EventHandler
import javafx.scene.input.{MouseButton, MouseEvent}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color.White

/**
  * Created by sergio on 4/29/17.
  */
object ImageWindow extends JFXApp {

    buildImage()

    private def buildImage() : Unit = {
        val N = 10
        val imgPath = "valle_luna.png"
        ImageReader.loadImage(imgPath)
        Geometry.delaunay(List.empty[Point],removeInitial = false)
        /*for(i <- 0 to N; j <- 0 to N){
            val (x,y) = (50 + 50*i, 50 + 50*j)
            println("Inserting point ",x,y)
            Geometry.addPointToTriangulation(new Point(x,y,ImageReader.getScalarValue(x,y)))
        }*/
    }

    stage = new PrimaryStage {
        title = "Delaunay Triangulation"
        width = 600
        height = 620
        scene = new Scene {
            fill = White

            def draw(): Unit = {
                content.clear()
                val cv = new Canvas(600, 600)
                cv.getGraphicsContext2D.drawImage(Geometry.getScalarAsImage, 0, 0)
                content.add(cv)
                val triangles = Geometry.getTriangles
                for (e <- triangles.flatMap(_.draw())) content.add(e)
            }

            onMouseClicked = new EventHandler[MouseEvent] {
                override def handle(event: MouseEvent){
                    event.getButton match {
                        case MouseButton.PRIMARY => {
                            val p = new Point(event.getX.toInt, event.getY.toInt, ImageReader.getScalarValue(event.getX.toInt,event.getY.toInt))
                            Geometry.addPointToTriangulation(p)
                            draw()
                        }
                        case _ => ;
                    }
                }
            }
        }
    }
}

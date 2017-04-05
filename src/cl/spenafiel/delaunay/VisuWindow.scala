package cl.spenafiel.delaunay

import javafx.event.EventHandler

import scala.io.StdIn
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import javafx.scene.input.MouseEvent

import scala.collection.mutable.ListBuffer
import scalafx.scene.paint.Color._
import scalafx.Includes._
import javafx.scene.input.MouseButton

object VisuWindow extends JFXApp {

    print("Numero de triangulos? ")
    val N = 0
    val ti = System.currentTimeMillis()
    val points  = Geometry createRandomPoints N
    var triangles = Geometry delaunay points.toList
    println("Delaunay " + N + " points takes " + (System.currentTimeMillis() - ti) + "ms")

    val ti2 = System.currentTimeMillis

    stage = new PrimaryStage {
        title = "Delaunay Trinagulation"
        width = 600
        height = 620
        scene = new Scene {
            fill = White
            def draw() : Unit = {
                content.clear()
                for (e <- triangles.flatMap(_.draw())) content.add(e)
                for (e <- points.flatMap(_.draw())) content.add(e)
            }
            println("Drawing process takes " + (System.currentTimeMillis() - ti) + "ms")
            onMouseClicked = new EventHandler[MouseEvent] {
                override def handle(event: MouseEvent){
                    event.getButton match {
                        case MouseButton.PRIMARY => {
                            points += new Point(event.x, event.y)
                            triangles = Geometry.delaunay(points.toList)
                            draw()
                        }
                        case MouseButton.SECONDARY => {
                            val circ = Geometry.getInCircle(new Point(event.x, event.y), triangles)
                            content.add(circ)
                        }
                        case _ => ;
                    }
                }
            }
        }
    }
}
package cl.spenafiel.delaunay

import javafx.event.EventHandler

import scala.io.StdIn
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}

import scala.collection.mutable.ListBuffer
import scalafx.scene.paint.Color._
import scalafx.Includes._

object VisuWindow extends JFXApp {

    val N = 0
    val ti = System.currentTimeMillis()
    val points  = Geometry createRandomPoints N
    var triangles = Geometry delaunay points.toList
    println("Delaunay takes " + (System.currentTimeMillis() - ti) + "ms")

    val ti2 = System.currentTimeMillis

    stage = new PrimaryStage {
        title = "Delaunay Triangulation"
        width = 600
        height = 620
        scene = new Scene {
            fill = White

            def draw() : Unit = {
                content.clear()
                for (e <- triangles.flatMap(_.draw())) content.add(e)
                for (e <- points.flatMap(_.draw())) content.add(e)
            }
            draw()
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

            onKeyPressed = new EventHandler[KeyEvent] {
                override def handle(event: KeyEvent): Unit = {
                    event.getCode match {
                        case KeyCode.R => {
                            points.clear()
                            triangles = ListBuffer.empty[Triangle].toList
                            draw()
                        }
                        case KeyCode.P => {
                            points += new Point(math.random * 600, math.random * 600)
                            triangles = Geometry.delaunay(points.toList)
                            draw()
                        }
                        case KeyCode.G => {
                            points.clear()
                            for(t <- Geometry.createGridPoints(20)) points += t
                            triangles = Geometry.delaunay(points.toList)
                            draw()
                        }
                        case _ => ;
                    }
                }
            }

        }
    }
}
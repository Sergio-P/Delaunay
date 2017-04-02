package cl.spenafiel.delaunay

import scala.io.StdIn
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color._

object VisuWindow extends JFXApp {

    print("Numero de triangulos? ")
    val N = StdIn.readInt()
    val ti = System.currentTimeMillis()
    val points = Geometry.createRandomPoints(N)
    val triangles = Geometry.delaunay(points.toList)
    println("Delaunay " + N + " points takes " + (System.currentTimeMillis() - ti) + "ms")

    val ti2 = System.currentTimeMillis()

    stage = new PrimaryStage {
        title = "Delaunay Trinagulation"
        width = 600
        height = 620
        scene = new Scene {
            fill = White
            for(e <- triangles.flatMap(_.draw())) content.add(e)
            for(e <- points.flatMap(_.draw())) content.add(e)
            println("Drawing process takes " + (System.currentTimeMillis() - ti) + "ms")
        }
    }
}
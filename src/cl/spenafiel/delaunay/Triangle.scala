package cl.spenafiel.delaunay

import scalafx.scene.Node
import scalafx.scene.shape.Line

/**
  * Created by sergio on 3/25/17.
  */
class Triangle(val a : Point, val b : Point, val c : Point) extends Drawable{

    override def draw(): Seq[Node] = {
        Seq(
            new Line{
                startX = a.x
                startY = a.y
                endX = b.x
                endY = b.y
            },
            new Line{
                startX = b.x
                startY = b.y
                endX = c.x
                endY = c.y
            },
            new Line{
                startX = c.x
                startY = c.y
                endX = a.x
                endY = a.y
            }
        )
    }

    def hasVertex(x : Point) : Boolean = {
        a == x || b == x || c == x
    }

}
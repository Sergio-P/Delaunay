package cl.spenafiel.delaunay

import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

/**
  * Created by sergio on 3/25/17.
  */
class Point(val x : Double = 0, val y : Double = 0, val value : Int = 0) extends Drawable{

    override def draw(): Seq[Node] = {
        Seq(new Circle {
            radius = 2
            centerX = x
            centerY = y
            fill = Color.Red
        })
    }

    override def equals(obj: scala.Any): Boolean = {
        obj match {
            case other : Point => other.x == x && other.y == y
            case _ => false
        }
    }

    override def toString: String = {
        "(" + x.toInt + "," + y.toInt + ")"
    }

}
package cl.spenafiel.delaunay

import scalafx.scene.Node

/**
  * Created by sergio on 3/25/17.
  */
trait Drawable {

    def draw() : Seq[Node]

}

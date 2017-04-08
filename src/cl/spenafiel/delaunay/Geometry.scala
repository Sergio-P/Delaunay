package cl.spenafiel.delaunay

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}

/**
  * Created by sergio on 3/25/17.
  */
object Geometry {

    private var nextTo : mutable.HashMap[Triangle, (Option[Triangle], Option[Triangle], Option[Triangle])] = mutable.HashMap.empty[Triangle, (Option[Triangle], Option[Triangle], Option[Triangle])]
    /**
      * Computes distance between 2 points
      *
      * @param a First point
      * @param b Second point
      * @return Distance between a and b
      */
    def distance(a: Point, b: Point): Double = {
        val dx = a.x - b.x
        val dy = a.y - b.y
        math.sqrt(dx * dx + dy * dy)
    }

    private def circleTest(t: Triangle, target: Point): Boolean = {
        val mab = -1 * (t.a.x - t.b.x) / (t.a.y - t.b.y)
        val mbc = -1 * (t.b.x - t.c.x) / (t.b.y - t.c.y)
        val nab = (t.a.y + t.b.y) / 2 - mab * (t.a.x + t.b.x) / 2
        val nbc = (t.b.y + t.c.y) / 2 - mbc * (t.b.x + t.c.x) / 2
        val cx = (nab - nbc) / (mbc - mab)
        val cy = mab * cx + nab
        val r2 = (cx - t.a.x) * (cx - t.a.x) + (cy - t.a.y) * (cy - t.a.y)
        (target.x - cx) * (target.x - cx) + (target.y - cy) * (target.y - cy) < r2
    }

    private def orientation(segA: Point, segB: Point, target: Point): Double = {
        (target.x - segB.x) * (segA.y - segB.y) - (segA.x - segB.x) * (target.y - segB.y)
    }

    /**
      * Makes n random points
      *
      * @param n The number of points
      * @return sequence of points
      */
    def createRandomPoints(n: Int, w: Int = 600): ListBuffer[Point] = {
        val a = new ListBuffer[Point]
        for (_ <- 1 to n) a += new Point(Math.random() * w, Math.random() * w)
        a
    }

    /**
      * Makes a grid if n x n points
      *
      * @param n The size of the grid
      * @return sequence of points
      */
    def createGridPoints(n: Int, w: Int = 600): ListBuffer[Point] = {
        val a = new ListBuffer[Point]
        val gap = w/n
        for (i <- 1 to n; j <- 1 to n) a += new Point(gap*(i-0.5),gap*(j-0.5))
        a
    }

    private def findIn(triangles: List[Triangle], point: Point): Option[Triangle] = {
        for (triangle <- triangles) {
            val (oab, obc, oca) = (orientation(triangle.a, triangle.b, point) < 0,
                orientation(triangle.b, triangle.c, point) < 0,
                orientation(triangle.c, triangle.a, point) < 0)
            if (oab == obc && obc == oca)
                return Some(triangle)
        }
        None
    }

    /**
      * Computes the Delaunay Triangulation
      *
      * @param points the points to be triangulated
      * @return A list of the triangles of triangulation
      */
    def delaunay(points: List[Point]): List[Triangle] = {
        val topLeft = new Triangle(new Point(-150, -100), new Point(-150, 700), new Point(750, -100))
        val bottomRight = new Triangle(new Point(750, 700), new Point(750, -100), new Point(-150, 700))

        val triangles = ListBuffer.empty[Triangle]
        triangles += topLeft
        triangles += bottomRight

        nextTo = mutable.HashMap.empty[Triangle, (Option[Triangle], Option[Triangle], Option[Triangle])]
        nextTo += topLeft -> (Some(bottomRight), None, None)
        nextTo += bottomRight -> (Some(topLeft), None, None)

        def updateNeighbour(tr: Option[Triangle], otherTr: Triangle): Unit = {
            tr.foreach { tr =>
                val prev = nextTo.getOrElse(tr, (None, None, None))

                if (!(otherTr hasVertex tr.a)) {
                    nextTo += tr -> (Some(otherTr), prev._2, prev._3)
                }
                else if (!(otherTr hasVertex tr.b)) {
                    nextTo += tr -> (prev._1, Some(otherTr), prev._3)
                }
                else if (!(otherTr hasVertex tr.c)) {
                    nextTo += tr -> (prev._1, prev._2, Some(otherTr))
                }
                else{
                    println("ERROR ACTUALIZAR VECINO")
                }
            }
        }

        def findWithSegment(tuple : (Option[Triangle], Option[Triangle], Option[Triangle]), a : Point, b : Point) : Option[Triangle] = {
            tuple._1 match {
                case Some(t) => if (t.hasVertex(a) && t.hasVertex(b)) return tuple._1
                case None => ;
            }
            tuple._2 match {
                case Some(t) => if (t.hasVertex(a) && t.hasVertex(b)) return tuple._2
                case None => ;
            }
            tuple._3 match {
                case Some(t) => if (t.hasVertex(a) && t.hasVertex(b)) return tuple._3
                case None => ;
            }
            None
        }

        def flipDiag(ot1: Option[Triangle], t2: Triangle, p: Point): Unit = {
            ot1 match {
                case Some(t1) => if (circleTest(t1, p)) {
                    val optOther = t1.getVertices.find(p => !(t2 hasVertex p))
                    if (optOther.isEmpty) {
                        println("ERROR FATAL TRIANGULOS NO VECINOS")
                        return
                    }
                    val other = optOther.get
                    val side1 = t1.nextVertexCCW(other)
                    val side2 = t1.nextVertexCCW(side1)

                    val nt1 = new Triangle(p, other, side1)
                    val nt2 = new Triangle(other, p, side2)

                    val next1 = nextTo.getOrElse(t1, (None, None, None))
                    val next2 = nextTo.getOrElse(t2, (None, None, None))

                    val nextOtherSide1 = findWithSegment(next1, other, side1)
                    val nextOtherSide2 = findWithSegment(next1, other, side2)
                    val nextPSide1 = findWithSegment(next2, p, side1)
                    val nextPSide2 = findWithSegment(next2, p, side2)

                    nextTo += nt1 -> (nextOtherSide1, nextPSide1, Some(nt2))
                    updateNeighbour(nextOtherSide1,nt1)
                    updateNeighbour(nextPSide1,nt1)

                    nextTo += nt2 -> (nextPSide2, nextOtherSide2, Some(nt1))
                    updateNeighbour(nextPSide2,nt2)
                    updateNeighbour(nextOtherSide2,nt2)

                    if(!(triangles contains t1)){
                        println("ERROR Triangulo no válido")
                    }
                    triangles -= t1
                    triangles -= t2
                    nextTo -= t1
                    nextTo -= t2
                    triangles += nt1
                    triangles += nt2

                    flipDiag(nextOtherSide1, nt1, p)
                    flipDiag(nextOtherSide2, nt2, p)
                }
                case None => ;
            }
        }

        def addPointToTriangulation(point : Point): Unit ={
            val t = findIn(triangles.toList, point)
            t.foreach { tr =>
                val trAB = new Triangle(tr.a, tr.b, point)
                val trBC = new Triangle(tr.b, tr.c, point)
                val trCA = new Triangle(tr.c, tr.a, point)

                triangles += trAB
                triangles += trBC
                triangles += trCA
                triangles -= tr

                val ntr = nextTo.get(tr)
                nextTo -= tr

                ntr.foreach { tuple =>
                    nextTo += trAB -> (Some(trBC), Some(trCA), tuple._3)
                    nextTo += trBC -> (Some(trCA), Some(trAB), tuple._1)
                    nextTo += trCA -> (Some(trAB), Some(trBC), tuple._2)
                    updateNeighbour(tuple._3, trAB)
                    updateNeighbour(tuple._1, trBC)
                    updateNeighbour(tuple._2, trCA)
                    flipDiag(tuple._3, trAB, point)
                    flipDiag(tuple._1, trBC, point)
                    flipDiag(tuple._2, trCA, point)
                }
            }
        }

        for (point <- points) {
            addPointToTriangulation(point)
        }

        for (triangle <- triangles){
            if((triangle hasVertex bottomRight.a) || (triangle hasVertex bottomRight.b) ||
                (triangle hasVertex topLeft.a) || (triangle hasVertex topLeft.b)){
                triangles -= triangle
            }
        }

        println("Total " + triangles.length)
        triangles.toList
    }

    def getInCircle(point : Point, triangles : Seq[Triangle]): Node ={
        println(point)
        val t = findIn(triangles.toList, point)
        t.map { t =>
            val mab = -1 * (t.a.x - t.b.x) / (t.a.y - t.b.y)
            val mbc = -1 * (t.b.x - t.c.x) / (t.b.y - t.c.y)
            val nab = (t.a.y + t.b.y) / 2 - mab * (t.a.x + t.b.x) / 2
            val nbc = (t.b.y + t.c.y) / 2 - mbc * (t.b.x + t.c.x) / 2
            val cx = (nab - nbc) / (mbc - mab)
            val cy = mab * cx + nab
            val r2 = (cx - t.a.x) * (cx - t.a.x) + (cy - t.a.y) * (cy - t.a.y)
            return new Circle{
                centerX = cx
                centerY = cy
                radius = math.sqrt(r2)
                stroke = Color.Blue
                fill = Color.Transparent
            }
        }
        Circle(-1,-1,1)
    }

    def getNeighs() : Seq[Node] = {
        val s = ListBuffer.empty[Node]
        for((tr,tp) <- nextTo){
            if (tp._1.isDefined) {
                println("V1 definido")
                s += new Line {
                    startX = tr.a.x
                    startY = tr.a.y
                    endX = tp._1.get.a.x
                    endY = tp._1.get.a.y
                    stroke = Color.Blue
                }
            }
            if (tp._2.isDefined) {
                println("V2 definido")
                s += new Line {
                    startX = tr.b.x
                    startY = tr.b.y
                    endX = tp._2.get.b.x
                    endY = tp._2.get.b.y
                    stroke = Color.Blue
                }
            }
            if (tp._3.isDefined) {
                println("V3 definido")
                s += new Line {
                    startX = tr.c.x
                    startY = tr.c.y
                    endX = tp._3.get.c.x
                    endY = tp._3.get.c.y
                    stroke = Color.Blue
                }
            }
        }
        s.toList
    }

}

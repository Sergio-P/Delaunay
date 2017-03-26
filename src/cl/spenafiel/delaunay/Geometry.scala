package cl.spenafiel.delaunay

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by sergio on 3/25/17.
  */
object Geometry {

    /**
      * Computes distance between 2 points
      * @param a First point
      * @param b Second point
      * @return Distance between a and b
      */
    def distance(a: Point, b: Point): Double = {
        val dx = a.x - b.x
        val dy = a.y - b.y
        math.sqrt(dx * dx + dy * dy)
    }

    private def circleTest(t : Triangle, target : Point): Boolean ={
        val mab = -1 * (t.a.y - t.b.y) / (t.a.x - t.b.x)
        val mbc = -1 * (t.b.y - t.c.y) / (t.b.x - t.c.x)
        val nab = (t.a.y + t.b.y)/2 - mab * (t.a.x + t.b.x)/2
        val nbc = (t.b.y + t.c.y)/2 - mbc * (t.b.x + t.c.x)/2
        val cx = (nab - nbc) / (mab - mbc)
        val cy = mab * cx + nab
        (target.x - cx)*(target.x - cx) + (target.y - cy)*(target.y - cy) < cx*cx + cy*cy
    }

    private def orientation(segA : Point, segB : Point, target :Point) : Double = {
        (target.x - segB.x) * (segA.y - segB.y) - (segA.x - segB.x) * (target.y - segB.y)
    }

    /**
      * Makes n random points
      * @param n The number of points
      * @return sequence of points
      */
    def createRandomPoints(n: Int, w : Int = 600): Seq[Point] = {
        for (_ <- 1 to n) yield new Point(Math.random()*w, Math.random()*w)
    }

    private def findIn(triangles: ListBuffer[Triangle], point: Point) : Option[Triangle] = {
        for(triangle <- triangles){
            val (oab, obc, oca) = (orientation(triangle.a,triangle.b,point) < 0,
                orientation(triangle.b,triangle.c,point) < 0,
                orientation(triangle.c,triangle.a,point) < 0)
            if (oab == obc && obc == oca)
                return Some(triangle)
        }
        None
    }

    /**
      * Computes the Delaunay Triangulation
      * @param points the points to be triangulated
      * @return A list of the triangles of triangulation
      */
    def delaunay(points: List[Point]) : List[Triangle] = {
        val topLeft     = new Triangle(new Point(0,0), new Point(0,600), new Point(600,0))
        val bottomRight = new Triangle(new Point(600,600), new Point(600,0), new Point(0,600))

        val triangles  = ListBuffer.empty[Triangle]
        triangles += topLeft
        triangles += bottomRight

        val nextTo = mutable.HashMap.empty[Triangle,(Option[Triangle],Option[Triangle],Option[Triangle])]
        nextTo += topLeft -> (Some(bottomRight), None, None)
        nextTo += bottomRight -> (Some(topLeft), None, None)

        def updateNeighbour(tr : Option[Triangle], otherTr : Triangle) : Unit = {
            tr.foreach {tr =>
                val prev = nextTo.getOrElse(tr,(None,None,None))

                if(!(otherTr hasVertex tr.a))
                    nextTo += tr -> (Some(otherTr), prev._2, prev._3)

                else if(!(otherTr hasVertex tr.b))
                    nextTo += tr -> (prev._1, Some(otherTr), prev._3)

                else if(!(otherTr hasVertex tr.c))
                    nextTo += tr -> (prev._1, prev._2, Some(otherTr))
            }
        }

        for(point <- points){
            val t = findIn(triangles, point)
            t.map { tr =>
                val trAB = new Triangle(tr.a,tr.b,point)
                val trBC = new Triangle(tr.b,tr.c,point)
                val trCA = new Triangle(tr.c,tr.a,point)

                nextTo.get(tr).foreach { tuple =>
                    nextTo += trAB -> (Some(trBC),Some(trCA),tuple._3)
                    nextTo += trBC -> (Some(trCA),Some(trAB),tuple._1)
                    nextTo += trCA -> (Some(trAB),Some(trBC),tuple._2)
                    updateNeighbour(tuple._3,trAB)
                    updateNeighbour(tuple._1,trBC)
                    updateNeighbour(tuple._2,trCA)
                }

                nextTo -= tr
                triangles += trAB
                triangles += trBC
                triangles += trCA
                triangles -= tr
            }
        }

        triangles.toList
    }

}

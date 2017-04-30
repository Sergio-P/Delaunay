package cl.spenafiel.delaunay

import scalafx.scene.image.{Image, PixelReader}


/**
  * Created by sergio on 4/29/17.
  */
object ImageReader {

    var scalar : Array[Array[Int]] = Array.ofDim(0,0)
    var width = 0
    var height = 0

    def loadImage(path : String) : Unit = {
        val image = new Image(path)
        width = image.getWidth.toInt
        height = image.getHeight.toInt
        scalar = Array.ofDim(width, height)
        image.pixelReader match {
            case Some(pr) => {
                for(i <- 0 until width; j <- 0 until height)
                    scalar(i)(j) = pr.getArgb(i,j)
            }
            case None => ;
        }
    }

    def error(x: Int, y: Int) : Double = {
        val errR = math.abs(x%256 - y%256)
        val errG = math.abs((x/256)%256 - (y/256)%256)
        val errB = math.abs((x/256/256)%256 - (y/256/256)%256)
        errR + errG + errB
    }

    def searchMaxError(apScalar : Array[Array[Int]]): (Int,Int) ={
        var maxi = -1
        var maxj = -1
        var maxErr = -1.0

        for(i <- 0 until width; j <- 0 until height) {
            val err = error(apScalar(i)(j), scalar(i)(j))
            if(err > maxErr){
                maxi = i
                maxj = j
                maxErr = err
            }
        }

        (maxi, maxj)
    }

    def getScalarValue(i : Int, j : Int) : Int = {
        scalar(i)(j)
    }

}

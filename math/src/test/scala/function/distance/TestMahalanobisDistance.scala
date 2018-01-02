package function.distance

import java.io.File
import java.util.Random

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.distance.MahalanobisDistance
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis._
import breeze.linalg.{DenseMatrix, eigSym, inv, svd}
import inference.{MardiaIris, TestManovaData}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class TestMahalanobisDistance extends FlatSpec with Matchers {

  "MahalanobisDistance" should "calculate distance for two vectors" in new TestManovaData {
    val (groups, data) = readNonStandardised()

    val X = data(0,::).inner
    val Y = data(3,::).inner
    // note

    println(s"X: ${X.toString}")
    println(s"Y: ${Y.toString}")

    val distance = MahalanobisDistance(X,Y)
    println(s"Distance: $distance")
    distance should not be(0)
  }

  "MahalanobisDistance" should "calculate distance and approximate parameters" in new TestManovaData {
    val (groups, data) = read()
    val distance = MahalanobisDistance(data)
    println(distance)

    println(s"Rows: ${distance.rows} Cols: ${distance.cols}")

  }

  "MahalanobisDistance" should "calculate distance between two matrices" in new TestManovaData {
    val (groups, data) = read()
    val distance = MahalanobisDistance(data, data)
    println(distance)
    // in this case the distance will be 0
    println(s"Rows: ${distance.rows} Cols: ${distance.cols}")
    distance.toArray.sum should equal(0.0)
  }

  "MahalanobisDistance" should "calculate distance" in new MardiaIris {
    val (data) = read()
    val distance = MahalanobisDistance(data)
    println(distance)
  }


  "MahalanobisDistance" should "calculate distance between two matrices greater than 0" in new TestManovaData {
    val (groups, data) = read()
    val data2 = DenseMatrix.tabulate(data.rows, data.cols) {
      case (i,j) => 1.0+data(i,j)
    }
    val distance = MahalanobisDistance(data, data2)
    println(distance)
    // in this case the distance will be the same for each row.
    println(s"Rows: ${distance.rows} Cols: ${distance.cols}")
  }
}

package inference

import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis.MardiaTest
import breeze.linalg.DenseMatrix
import org.scalatest.{FlatSpec, Matchers}

class TestMardiaTest extends FlatSpec with Matchers {

  "Mardia" should "test iris data " in new MardiaIris {
    val data = read()
    val test = MardiaTest(0.05, data)

    println(s"$test")
  }

}

trait MardiaIris extends TestMardiaData {
  val fileName:String = "iris_virginica.csv"
  val cols = 0 to 3
}

trait TestMardiaData extends MatrixReader {

  val fileName:String

  val cols:Seq[Int]

  def read():DenseMatrix[Double] = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)
    // we know the headers on the first line
    // they are:
    val data = mat(::, cols).toDenseMatrix
    // the data set is standardised prior to the procedure
    val X = StandardisedNormalisation().transform(data)
    X
  }
}

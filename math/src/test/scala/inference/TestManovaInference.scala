package inference

import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.probability.analysis.{Manova, WilksLambda}
import breeze.linalg.DenseMatrix
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class TestManovaInference extends FlatSpec
  with Matchers{

  /**
    * The expected output should correspond with the R method.
    *
    *
sROC 0.1-2 loaded
                 Df     Wilks approx F num Df den Df    Pr(>F)
as.factor(Group)  4 0.0021936   27.666     36 241.57 < 2.2e-16 ***
Residuals        72



    */
  "Manova" should "test wilks lambda" in new TestManovaData {

    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=WilksLambda())

    println(result)

    result.manovaStat.df1 should equal(36)
    // are we in the ball park?
    Math.round(result.manovaStat.stat) <= 28 should be(true)
    Math.round(result.manovaStat.stat) >= 27 should be(true)

  }


}


trait TestManovaData extends MatrixReader {

  val fileName="test_mandible_data.csv"

  override val skipHeader = true

  def read():(List[String], DenseMatrix[Double]) = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)
    // we know the headers on the first line
    // they are:
    // "Case","Group","X1","X2","X3","X4","X5","X6","X7","X8","X9","Sex"
    // we want to keep columns 1 .. 10 and discard the other two.
    val m2 = mat(::, 1 to 10).toDenseMatrix
    // we also know ahead of time that there are 5 groups in the data.
    val groups = mat(::,0).toArray.map(_.toString).toList
    (groups, m2)
  }
}

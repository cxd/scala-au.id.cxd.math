package probability.regression

import au.id.cxd.math.probability.regression.OrdLeastSquares
import breeze.linalg.DenseVector
import org.scalatest._

/**
 * Created by cd on 29/06/2014.
 */
class TestOrdLeastSquares extends FlatSpec with ShouldMatchers {

  "OrdLeastSquares" should "approximate sin" in {
    def testY(X:DenseVector[Double]) = {
      X.map(x => Math.sin(x))
    }
    val X1 = Array(0.0, Math.PI/2.0, Math.PI, Math.PI*2.0)
    val X2 = DenseVector(X1)
    val Y = testY(X2)
    val ols = OrdLeastSquares(X2, Y, 3, 0.5)
    val T1 = ols.train()
    val error = T1._2
    (error <= 0.5) should be (true)
    println(Y)
    println(T1)
  }

}

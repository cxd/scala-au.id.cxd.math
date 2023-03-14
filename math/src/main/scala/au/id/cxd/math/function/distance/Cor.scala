package au.id.cxd.math.function.distance

import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg.DenseMatrix

/**
  * produce the correlation matrix for A and B
  * @param a
  * @param b
  */
class Cor  (a:DenseMatrix[Double], b:DenseMatrix[Double])  extends Cov (a,b) {

}
object Cor {
  def apply(a:DenseMatrix[Double]) = {
    val scaleA = StandardisedNormalisation().transform(a)
    new Cor(scaleA, scaleA).op()
  }
  def apply(a:DenseMatrix[Double], b:DenseMatrix[Double]) = {
    val scaleA = StandardisedNormalisation().transform(a)
    val scaleB = StandardisedNormalisation().transform(b)
    new Cor(scaleA, scaleB).op()
  }
}

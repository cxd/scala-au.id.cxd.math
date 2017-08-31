package au.id.cxd.math.function

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
    val scaleA = new StandardisedNormalisation().transform(a)
    new Cor(scaleA, scaleA).op()
  }
  def apply(a:DenseMatrix[Double], b:DenseMatrix[Double]) = {
    val scaleA = new StandardisedNormalisation().transform(a)
    val scaleB = new StandardisedNormalisation().transform(b)
    new Cor(scaleA, scaleB).op()
  }
}

package au.id.cxd.math.function.column

import breeze.linalg.{*, DenseMatrix, sum}

/**
  * columnwise point estimate for standard deviation for attributes in a matrix
  */
class ColStddev (m:DenseMatrix[Double]) {

  def op(): DenseMatrix[Double] = {
    val means = ColMeans (m)
    val delta = m.mapPairs {
      (ind, value) =>
        val mu = means(0,ind._2)
        Math.pow(value - mu, 2.0)
    }
    val colSums = sum(delta(::,*)).inner.toDenseMatrix
    val n = m.rows.toDouble
    colSums / (n-1.0)
  }
}
object ColStddev {
  def apply(m:DenseMatrix[Double]) =
    new ColStddev(m) op
}

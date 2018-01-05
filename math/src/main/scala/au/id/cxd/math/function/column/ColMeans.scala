package au.id.cxd.math.function.column

import breeze.linalg.{*, DenseMatrix, DenseVector, sum}

/**
  * a column means operation over a dense matrix
  * return the result as a single row matrix
  */
class ColMeans(m:DenseMatrix[Double]) {

  def op():DenseMatrix[Double] = {
    val n = m.rows
    val meanVector = DenseVector.tabulate[Double](m.cols) {
      j => {
        val col = m(::,j)
        val seed = 0.0
        val total = col.foldLeft(seed) { (a, b) => a + b }
        total/n
      }
    }
    meanVector.toDenseMatrix
  }

}

object ColMeans {
  /**
    * perform the columnmeans operation
    * @param m
    * @return
    */
  def apply(m:DenseMatrix[Double]) =
    new ColMeans(m) op()
}

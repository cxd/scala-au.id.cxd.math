package au.id.cxd.math.function.column

import breeze.linalg.{*, DenseMatrix, sum}

/**
  * a column means operation over a dense matrix
  * return the result as a single row matrix
  */
class ColMeans(m:DenseMatrix[Double]) {

  def op():DenseMatrix[Double] = {
    val colsums = sum(m(::,*)).inner
    val n = m.rows.toDouble
    (colsums / n).toDenseMatrix
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

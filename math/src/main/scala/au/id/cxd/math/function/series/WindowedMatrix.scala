package au.id.cxd.math.function.series

import breeze.linalg.DenseMatrix

trait WindowedMatrix extends Sliding {

  private def process[T](i:Int, j:Int, mat:DenseMatrix[T], item:T):DenseMatrix[T] = {
    mat(i,j) = item
    mat
  }

  /**
    * given a series produce a new matrix of
    * rows = series.length - stride + 1
    * cols = stride.
    *
    * @param state
    * @param series
    * @param stride
    * @param by
    * @tparam T
    * @return
    */
  def windowedMatrix(series: Seq[Double], stride: Int, by: Int = 1):DenseMatrix[Double] = {
    val nrows = series.size - stride + 1
    val cols = stride
    val newMat:DenseMatrix[Double] = DenseMatrix.zeros[Double](nrows, cols)
    window(newMat, series, stride, by)(process(_,_,_,_))
  }

}

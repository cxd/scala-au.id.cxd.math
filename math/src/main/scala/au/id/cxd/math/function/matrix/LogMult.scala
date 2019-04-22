package au.id.cxd.math.function.matrix

import breeze.linalg.DenseMatrix

class LogMult {

  def op(A: DenseMatrix[Double], B: DenseMatrix[Double]): DenseMatrix[Double] = {
    if (A.cols != B.rows) throw new Exception(s"Matrices do not conform, A.cols (${A.cols}) != B.rows (${B.rows})")
    DenseMatrix.tabulate(A.rows, B.cols) {
      case (i, j) =>
        // sum A row * B.col
        val a = A(i, ::).inner.toArray
        val b = B(::, j).toArray
        val total = a.zip(b).foldLeft(0.0) {
          (z, pair) => {
            val logA = if (pair._1 != 0) Math.log(Math.exp(pair._1)) else 0.0
            val logB = if (pair._2 != 0) Math.log(Math.exp(pair._2)) else 0.0
            logA + logB
          }
        }
        // convert out of log space
        if (total != 0) Math.exp(total)
        else total
    }
  }

}

object LogMult {
  def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
    new LogMult().op(a, b)
}

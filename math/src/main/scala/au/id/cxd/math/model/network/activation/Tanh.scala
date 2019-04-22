package au.id.cxd.math.model.network.activation

import breeze.linalg.DenseMatrix

case class Tanh() extends Activation {
  /**
    * the activation function applies to the input matrix.
    *
    * @param h
    * @return
    */
  override def apply(h: DenseMatrix[Double]): DenseMatrix[Double] =
    DenseMatrix.tabulate(h.rows, h.cols) {
      case (i, j) => Math.tanh(h(i, j))
    }

  /**
    * derivative of the activation function
    *
    * @param h
    * @return
    */
  override def derivative(h: DenseMatrix[Double]): DenseMatrix[Double] =
    DenseMatrix.tabulate(h.rows, h.cols) {
      case (i, j) => 1.0 - Math.tanh(h(i, j)) * Math.tanh(h(i, j))
    }
}

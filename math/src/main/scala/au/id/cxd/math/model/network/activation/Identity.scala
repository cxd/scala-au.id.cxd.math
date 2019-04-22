package au.id.cxd.math.model.network.activation

import breeze.linalg.DenseMatrix

class Identity() extends Activation {
  /**
    * the activation function applies to the input matrix.
    *
    * @param h
    * @return
    */
  override def apply(h: DenseMatrix[Double]): DenseMatrix[Double] = h

  /**
    * derivative of the activation function
    *
    * @param h
    * @return
    */
  override def derivative(h: DenseMatrix[Double]): DenseMatrix[Double] =
    DenseMatrix.tabulate(h.rows, h.cols) {
      case (i, j) =>
        if (h(i, j) > 0) 1.0
        else -1.0
    }
}
object Identity {
  def apply() = new Identity()
}
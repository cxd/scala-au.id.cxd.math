package au.id.cxd.math.model.network.activation
import breeze.linalg.DenseMatrix

/**
  * sigmoid function with slope and temperature parameters defaulting to 1.
  * @param slope
  */
case class Sigmoid(val slope:Double = 1.0, val temperature:Double = 1.0) extends Activation {

  private def sigma(x:Double):Double = 1.0 / (1.0 + Math.exp(-slope*x / temperature ))

  /**
    * the activation function applies to the input matrix.
    *
    * @param h
    * @return
    */
  override def apply(h: DenseMatrix[Double]): DenseMatrix[Double] = {
    val result = DenseMatrix.tabulate(h.rows, h.cols) {
      case (i, j) => sigma(h(i,j))
    }
    result
  }

  /**
    * derivative of the activation function
    *
    * @param h
    * @return
    */
  override def derivative(h: DenseMatrix[Double]): DenseMatrix[Double] = {
    val result = DenseMatrix.tabulate(h.rows, h.cols) {
      case (i, j) => -slope/temperature * sigma(h(i,j)) * (1.0 - sigma(h(i,j)))
    }
    result
  }
}
package au.id.cxd.math.model.network.activation
import breeze.linalg.{DenseMatrix, diag}

case class Softmax() extends Activation {
  /**
    * the activation function applies to the input matrix.
    *
    * @param h
    * @return
    */
  override def apply(h: DenseMatrix[Double]): DenseMatrix[Double] = {
    val m1 = DenseMatrix.tabulate(h.rows, h.cols) {
      case (i,j) => Math.exp(h(i,j))
    }
    val total = m1.data.sum
    1.0/total * m1
  }

  /**
    * derivative of the activation function
    *
    * @param h
    * @return
    */
  override def derivative(h: DenseMatrix[Double]): DenseMatrix[Double] = {
    // here we expect f to be a n x 1 vector.
    val f = apply(h)
    // calculate the matrix of kronecker delta - f(x)
    //
    val k = DenseMatrix.tabulate(f.rows, f.cols) {
      case(i,j) => if (i == j) 1.0
      else 0.0
    }
    val kf = k - f
    val d1 = f *:* kf
    d1
    //d1*h
  }
}

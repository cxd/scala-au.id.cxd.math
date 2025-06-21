package au.id.cxd.math.model.network.activation
import breeze.linalg.{DenseMatrix, diag}

case class Softmax(val temperature:Double=1.0) extends Activation {
  /**
    * the activation function applies to the input matrix.
    *
    * @param h
    * @return
    */
  override def apply(h: DenseMatrix[Double]): DenseMatrix[Double] = {
    val h1 = h/temperature
    val m1 = DenseMatrix.tabulate(h.rows, h.cols) {
      case (i,j) =>
        val maxH = h1(::,j).toArray.max
        Math.exp(h1(i,j) - maxH)
    }
    val total = m1.toArray.sum
    1.0/total * m1
  }

  /**
    * derivative of the activation function
    *
    * The derivative of the softmax function is a jacobian
    * and has a 2-d matrix form.
    *
    * @param h
    * @return
    */
  override def derivative(h: DenseMatrix[Double]): DenseMatrix[Double] = {
    // here we expect f to be a n x 1 vector.
    val f = apply(h)
    // calculate the matrix of kronecker delta - f(x)
    val e = DenseMatrix.ones[Double](1, h.rows)
    val k = DenseMatrix.tabulate(h.rows, h.rows) {
      case (i, j) =>
        if (i == j) 1.0
        else 0.0
    }
    val d1 = f * e
    val d2 = e.t * f.t
    val d3 = k - d2
    val d4 = d1 *:* d3
    d4
  }
}

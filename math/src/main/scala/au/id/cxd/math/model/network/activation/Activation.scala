package au.id.cxd.math.model.network.activation

import breeze.linalg.DenseMatrix

trait Activation {

  /**
    * the activation function applies to the input matrix.
    * @param h
    * @return
    */
  def apply(h:DenseMatrix[Double]):DenseMatrix[Double]

  /**
    * derivative of the activation function
    * @param h
    * @return
    */
  def derivative(h:DenseMatrix[Double]):DenseMatrix[Double]
}

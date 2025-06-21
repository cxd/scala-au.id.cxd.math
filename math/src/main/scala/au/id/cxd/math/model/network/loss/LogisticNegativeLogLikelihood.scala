package au.id.cxd.math.model.network.loss

import au.id.cxd.math.model.network.activation.Softmax
import breeze.linalg.{DenseMatrix, sum}
import breeze.numerics.log


/**
  * For softmax multinomial regression calculate the negative log likelihood
  *
  */
case class LogisticNegativeLogLikelihood(val logits:Boolean=false, val temperature:Double=1.0) extends Loss {

  val stable = 1e-15

  /**
    * The full error gradient is calculated as
    * grad = (probs - y_onehot).t * X
    * where X is the input data array
    * in this case we calculate only the (probs - y.onehot) component
    * since the dot product with X will occur inside of the backpropagation procedure during training of the SGD
    * @param obs
    * @param probs
    * @return
    */
  def errorGrad(obs:DenseMatrix[Double], probs:DenseMatrix[Double]):DenseMatrix[Double] = {
    val grad = (probs - obs).t
    grad
  }

  /**
    * calculate the negative log likelihood and the (probs - obs).t term
    * @param obs - assume this is 1 hot encoded target matrix
    * @param sim - this is either the logits (if logits == true) or probabilities.
    *            if logits they will be converted to probabilities internally using softmax.
    *  @return
    */
  override def apply(obs: DenseMatrix[Double], sim: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
    val y_hat = if (logits) {
      Softmax(temperature=temperature).apply(sim)
    } else obs
    val log_probs = log(y_hat + stable)
    val log_likelihood = sum(sim * log_probs)
    val grad = errorGrad(obs, y_hat)
    (-log_likelihood, grad)
  }

}

package au.id.cxd.math.probability.random

import au.id.cxd.math.probability.continuous.Beta

/**
  * A random variate from the beta distribution Beta(alpha, beta)
  *
  * Given in numerical recipes as being drawn from two separate RGamma variates
  *
  * x \sim Gamma(alpha, 1)
  * y \sim Gamma(beta, 1)
  * x / (x + y) \sim Beta(alpha, beta)
  *
  * @param alpha
  * @param beta
  */
class RBeta(override val alpha:Double, override val beta:Double) extends Beta(alpha, beta) with RandomDeviate {

  val g1 = RGamma(alpha, 1.0)
  val g2 = RGamma(beta, 1.0)

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    val x = g1.draw()
    val y = g2.draw()
    x / (x + y)
  }
}
object RBeta {
  def apply(alpha:Double, beta:Double) = new RBeta(alpha,beta)
}
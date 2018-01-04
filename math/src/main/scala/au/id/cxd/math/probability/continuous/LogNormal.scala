package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.Constants

/**
  * log normal distribution
  * @param mu
  * @param sigma2
  */
class LogNormal(val mu: Double, val sigma2: Double) extends ContinuousDistribution {

  val sigma = Math.sqrt(sigma2)

  override def mean(): Double = Math.exp(mu + sigma2/2.0)
  /**
    * variance for the distribution $\sigma&#94;2$
    *
    * @return
    */
  override def variance(): Double = {
    val em1 = Math.exp(sigma2) - 1
    val em2 = Math.exp(2*mu + sigma2)
    em1*em2
  }

  /**
    * calculate the cdf $P( < y)$.
    *
    * @param y
    * @return
    */
  override def cdf(y: Double): Double = {
    if (Math.log(y).isNaN) 0.0
    else {
      val u = (Math.log(y) - mu) / sigma
      val P = Normal(0)(1).cdf(u)
      P
    }
  }

  override def invcdf(p: Double): Double = {
    if (p == 0.0) Double.PositiveInfinity
    else if (p == 1.0) 0.0
    else {
      val u = Normal(0)(1).invcdf(p)
      val x = Math.exp(mu + sigma *u)
      x
    }
  }

  /**
    * probability density function
    *
    * @param y
    * @return
    */
  override def pdf(y: Double): Double = {
    if (y <= 0.0) 0.0
    else {
      val u = (Math.log(y) - mu) / sigma
      val p = 1d / (y * Math.abs(sigma) * Math.sqrt(2.0 * Math.PI)) * Math.exp(-(u * u) / 2.0)
      p
    }
  }


}
object LogNormal {
  def apply(mu:Double, sigma2:Double) = new LogNormal(mu, sigma2)
}

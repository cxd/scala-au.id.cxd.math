package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.beta.{IncompleteBetaFn, InverseBeta}
import au.id.cxd.math.function.gamma.GammaFn

/**
  * #import MathJax
  *
  * @param df
  * @param mu
  * @param sigma
  */
class StudentT(val df: Double = 1.0, val mu: Double = 0.0, val sigma: Double = 1.0)
  extends ContinuousDistribution {

  /**
    * mean of the distribution
    * @return
    */
  override def mean(): Double = mu

  /**
    *
    * variance for the distribution
    * $$
    * Var[Student_{\nu,\mu,\sigma}] = \frac{\nu}{\nu - 2} \sigma&#94;2
    * $$
    * @return
    */
  override def variance(): Double = df / (df - 2.0) * sigma * sigma

  /**
    * calculate the cdf $P( < y)$.
    *
    * @param y
    * @return
    */
  override def cdf(y: Double): Double = {
    val x = df / (df + Math.sqrt((y-mu)/sigma))
    val p = 0.5 * IncompleteBetaFn(x, 0.5*df, 0.5)
    if (y >= mu) 1.0 - p
    else p
  }


  /**
    * inverse cdf of student t distribution
    *
    * @param p
    * @return
    */
  override def invcdf(p: Double): Double = {
    val mp = List(p, 1.0-p).min
    val x = InverseBeta(2.0*mp, 0.5*df, 0.5)
    val x1 = sigma * Math.sqrt(df * (1.0 - x)/x)
    if (p >= 0.5)
      mu + x1
    else mu - x1
  }

  /**
    * probability density function
    * $$
    * p(y) = \frac{\Gamma(1/2[\nu + 1])}{\Gamma(1/2\nu)\sqrt{\nu\pi}\sigma}\left( 1 + \frac{1}{\nu} \left[ \frac{y - \mu}{\sigma} \right]&#94;2\right)&#94;{-\frac{1}{2}(\nu + 1)}
    * $$
    * @param y
    * @return
    */
  override def pdf(y: Double): Double = {
    val num = GammaFn(0.5*(df+1.0))
    val den = GammaFn(0*5*df)*Math.sqrt(df*Math.PI)*sigma
    val loc2 = (y - mu)/sigma*(y - mu)/sigma
    val right = Math.pow(1 + 1.0/df * loc2, -0.5*(df+1))
    num / den * right
  }


}

object StudentT {
  def apply() = new StudentT()

  def apply(df: Double) = new StudentT(df)

  def apply(df: Double, mu: Double, sigma: Double) = new StudentT(df, mu, sigma)
}

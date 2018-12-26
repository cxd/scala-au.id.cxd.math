package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.approximate.NumericIntegral
import au.id.cxd.math.function.gamma.{Erfc, InvErf, InvErfc}
import breeze.linalg.{max, min}

import scala.math._

/**
  * ##import MathJax
  *
  * Created by cd on 9/09/2014.
  *
  * the normal distribution
  *
  * $$
  *   N(x; \mu, \sigma&#94;2) = \frac{1}{\sqrt{2\pi\sigma&#94;2}} \exp\left[\frac{1}{2\sigma&#94;2} (x - \mu)&#94;2 \right]
  * $$
  *
  * The cdf of the normal distribution
  * $$
  * P(<x) = \int_{-\infty}&#94;x p(x')dx' = \frac{1}{2} erfc\left(-\frac{1}{\sqrt{2}}\left[\frac{x-\mu}{\sigma}\right]\right)
  * $$
  *
  */
class Normal(mu: Double, varianceVal: Double) extends ContinuousDistribution {

  val sigma = Math.sqrt(varianceVal)

  def pdf(y: Double): Double = {
    (1.0 / (sigma*Math.sqrt(2.0 * Math.PI)) ) * Math.exp( (-(y-mu)*(y-mu)) / (2.0 * varianceVal))
  }

  def mean(): Double = mu

  def variance(): Double = varianceVal


  /**
    * The cdf of the normal distribution
    * $$
    * P(<x) = \int_{-\infty}&#94;x p(x')dx' = \frac{1}{2} erfc\left(-\frac{1}{\sqrt{2}}\left[\frac{x-\mu}{\sigma}\right]\right)
    * $$
    * @param y
    * @return
    */
  override def cdf(y:Double):Double =
    1.0/2.0 * Erfc (-1/Math.sqrt(2) *((y-mean)/sigma) )

  /**
    * the normal quantile function given as
    * $$
    * \psi(u) = \sqrt{2} erf&#94;{-1} (2u - 1) = -\sqrt{2} erfc&#94;{-1}(2u)
    * $$
    * @param p
    * @return
    */
  override def invcdf(p: Double): Double = {
      mu + sigma * -1.0*Math.sqrt(2) * InvErfc(2.0*p)
  }


}

object Normal {
  def apply(mu: Double)(variance: Double) = new Normal(mu, variance)



}

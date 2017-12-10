package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.approximate.NumericIntegral
import breeze.linalg.{max, min}
import breeze.numerics.erfc

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

  def pdf(y: Double): Double = {
    (1 / sqrt(variance * 2.0 * Pi)) * exp(-1.0 * pow(y - mu, 2.0) / (2.0 * variance))
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
  override def cdf(y:Double):Double = {
    1.0/2.0 * erfc (-1/Math.sqrt(2) *((y-mean)/Math.sqrt(variance)) )
  }




}

object Normal {
  def apply(mu: Double)(variance: Double) = new Normal(mu, variance)
}

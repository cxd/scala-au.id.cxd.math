package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.NumericIntegral

import scala.math._

/**
  * ##import MathJax
  *
  * Created by cd on 9/09/2014.
  *
  * Uniform distribution
  *
  * $f(y) = 1 / (max - min)$ if $min \leq y \leq max$
  * 0 otherwise
  *
  * CDF of uniform(a,b)
  * $$
  * P(y) = \begin{cases}
  *   0 & \text{if} y < a\\
  *   \frac{y-a}{b-a} & \text{if} y \in [a,b)\\
  *   1 & \text{y > b)
  * \end{cases}
  * $$
  *
  */
class Uniform(min: Double, max: Double) extends ContinuousDistribution {


  def pdf(y: Double): Double = {
    if (min <= y && max >= y) {
      1.0 / (max - min)
    } else 0.0
  }

  def mean(): Double = (max + min) / 2.0

  def variance(): Double = pow((max - min), 2.0) / 12.0

  /**
    * CDF of uniform(a,b)
    * $$
    * P(y) = \begin{cases}
    *   0 & \text{if} y < a\\
    *   \frac{y-a}{b-a} & \text{if} y \in [a,b)\\
    *   1 & \text{y > b)
    * \end{cases}
    * $$
    * @param y
    * @return
    */
  override def cdf(y:Double) = {
    if (y < min) 0.0
    else if (min <= y && y <= max)
      (y-min)/(max-min)
    else 1.0
  }

}

object Uniform {
  def apply(min: Double)(max: Double) = new Uniform(min, max)
}

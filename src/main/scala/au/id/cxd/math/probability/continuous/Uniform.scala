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
    * the integral of the distribution
    *
    * integral 1/(theta2 - theta1) dy
    *
    * = 1/(theta2-theta1)*y_upper - 1/(theta2-theta2)*ylower
    * @param start
    * @param end
    * @return
    */
  override def integral(start: Double, end: Double) = {
    val low = if (start >= min && start <= max) {
      start
    } else min
    val high = if (end >= min && end <= max) {
      end
    } else max
    NumericIntegral(low, high, pdf(_)).integrate()
  }
}

object Uniform {
  def apply(min: Double)(max: Double) = new Uniform(min, max)
}

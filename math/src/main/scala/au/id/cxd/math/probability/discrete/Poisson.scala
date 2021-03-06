package au.id.cxd.math.probability.discrete

import au.id.cxd.math.count.Factorial

import scala.math._

/**
  * ##import MathJax
  *
  * $$
  *   f(y; \lambda) = \lambda&#94;y \frac{\exp{ \left[ -\lambda \right] }}{y!}
  * $$
  *
  * $$
  *   \mu = \lambda
  * $$
  *
  * $$
  *   \sigma&#94;2 = \lambda
  * $$
  *
  * Created by cd on 7/09/2014.
  */
class Poisson(lambda: Double) extends DiscreteDistribution {

  def pdf(y: Double) = pow(lambda, y) * exp(-1.0 * lambda) / Factorial(y)

  def mean() = lambda

  def variance() = lambda
}

object Poisson {
  def apply(lambda: Double) = new Poisson(lambda)
}

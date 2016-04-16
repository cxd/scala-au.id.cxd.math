package au.id.cxd.math.probability.continuous

import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function.GammaFn

import scala.math._

/**
  * ##import MathJax
  *
  *
  * Gamma distribution
  *
  * $Gamma(alpha) = (alpha-1)!$
  *
  * $$
  * [y^alpha-1 * exp(-y/beta)]/[beta^alpha * Gamma(alpha)]
  * $$
  *
  * Note the constraints are
  *
  * alpha > 0
  * beta > 0
  * 0 <= y < infinity
  *
  * Created by cd on 11/09/2014.
  */
class Gamma(alpha: Double, beta: Double) extends ContinuousDistribution {

  val a = if (alpha == 0.0) 1.0
  else alpha
  val b = if (beta == 0.0) 1.0
  else beta


  /**
    * pdf function
    * equivalent to "dgamma" in R
    *
    * @param y
    * @return
    */
  def pdf(y: Double): Double = {
    val gamma = GammaFn(alpha)
    (pow(y, a - 1.0) * exp(-y / b)) / (pow(b, a) * gamma)
  }

  def mean(): Double = a * b


  def variance(): Double = a * pow(b, 2.0)

}

object Gamma {
  def apply(alpha: Double)(beta: Double) = new Gamma(alpha, beta)
}

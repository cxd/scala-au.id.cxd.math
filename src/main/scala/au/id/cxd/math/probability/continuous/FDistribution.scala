package au.id.cxd.math.probability.continuous

import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function.BetaFn

/**
 * The F distribution is the ratio of two chi-square distributions
 * with a numerator and denominator degree of freedom.
 *
 * The definition of f distribution is given as:
 *
 * $$
 *   f(x; d_1, d_2) = \frac{ \sqrt{ \frac { ( d_1 x)^d_1 d_2^d_2 }{(d_1x + d_2)^{d_1+d_2} } } } {x B(d_1/2, d_2/2) }
 * $$
 * where B is the beta function (from the gamma function)
 * $$
 *   B(a, b) = \frac{ (a - 1)!(b - 1)! }{ (a + b - 1)! }
 * $$
 *
 * The mean of the distribution is defined where denominator df > 2
 * $$
 *   \mu = \frac{ d_2 }{d_2 - 2}
 * $$
 *
 * And the variance of the distribution is defined where the denominator df > 4
 * $$
 *   \sigma^2 = \frac{ 2 d_2^2 (d_1 + d_2 - 2) }{ d_1 (d_2 - 2)^2 (d_2 - 4) }
 * $$
 *
 * The f distribution is most often used in testing hypothesis about unknown variance.
 *
 * Created by cd on 5/11/14.
 */
class FDistribution(val numeratorDf:Double, val denominatorDf:Double) extends ContinuousDistribution {

  /**
   * beta calculated from degrees of freedom
   */
  val beta = BetaFn(numeratorDf)(denominatorDf)

  def mean(): Double = if (denominatorDf > 2.0)
                        denominatorDf / (denominatorDf - 2.0)
                       else 0.0

  def stddev(): Double =
    if (denominatorDf > 4.0)
      (2*Math.pow(denominatorDf, 2.0) * (numeratorDf + denominatorDf - 2.0) ) / (numeratorDf * Math.pow(denominatorDf - 2.0, 2.0)*(denominatorDf - 4.0))
    else 0.0

  def integral(start: Double, end: Double): Double = approxIntegral(start, end)(pdf)

  /**
   * calculate the pdf of the f distribution
   * @param y
   * @return
   */
  def pdf(y: Double): Double = {
    val a = Math.pow(numeratorDf*y, numeratorDf) * Math.pow(denominatorDf, denominatorDf)
    val b = Math.pow(numeratorDf*y + denominatorDf, numeratorDf+denominatorDf)
    val c = Math.sqrt( a / b )
    val d = y * beta
    (c / d)
  }
}

object FDistribution {
  def apply(numeratorDf:Double, denominatorDf:Double) = new FDistribution(numeratorDf, denominatorDf)
}

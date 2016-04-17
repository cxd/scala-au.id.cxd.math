package au.id.cxd.math.probability.continuous

import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function.{NumericIntegral, GammaFn, BetaFn}

/**
  * ##import MathJax
  *
  * The F distribution is the ratio of two chi-square distributions
  * with a numerator $d_1$ and denominator $d_2$ degree of freedom.
  *
  * The definition of f distribution is given as:
  *
  * $$
  * f(x; d_1, d_2) = \frac{ \Gamma \left( \frac{ d_1 + d_2 }{2} \right) d_1&#94;{d_1/2} d_2&#94;{d_2/2} } {\Gamma(d_1/2)\Gamma(d_2/2) } \frac{ x&#94;{d_1/2 - 1} } {(d_1 + d_2x)&#94;{(d_1 + d_2)/2} }
  * $$
  *
  * $$
  * \frac{ d_1&#94;{d_1/2} d_2&#94;{d_2/2} x&#94;{d_1/2 - 1} } { (d_2 + d_1x)&#94;{(d_1+d_2)/2} B(d_1/2, d_2/2)}
  * $$
  *
  * where B is the beta function (from the gamma function)
  * $$
  * B(a, b) = \frac{ (a - 1)!(b - 1)! }{ (a + b - 1)! }
  * $$
  * and $x > 0$
  *
  * The mean of the distribution is defined where denominator df > 2
  * $$
  * \mu = \frac{ d_2 }{d_2 - 2}
  * $$
  *
  * And the variance $\sigma^2$
  *
  * of the distribution is defined where the denominator df > 4
  *
  * $$
  * \frac{ 2 {d_2&#94;2} (d_1 + d_2 - 2) }{ d_1 {(d_2 - 2)}&#94;2 (d_2 - 4) }
  * $$
  *
  * The f distribution is most often used in testing hypothesis about an unknown variance.
  *
  * Created by cd on 5/11/14.
  */
class FDistribution(val numeratorDf: Double, val denominatorDf: Double) extends ContinuousDistribution {

  /**
    * beta calculated from degrees of freedom
    */
  val beta = BetaFn(numeratorDf / 2.0)(denominatorDf / 2.0)

  def mean(): Double = if (denominatorDf != 2.0)
    denominatorDf / (denominatorDf - 2.0)
  else 0.0

  def variance(): Double =
    if (denominatorDf != 4.0)
      (2 * Math.pow(denominatorDf, 2.0) * (numeratorDf + denominatorDf - 2.0)) / (numeratorDf * Math.pow(denominatorDf - 2.0, 2.0) * (denominatorDf - 4.0))
    else 0.0


  /**
    * calculate the pdf of the f distribution
    *
    * @param y
    * @return
    */
  def pdf(y: Double): Double = {
    if (y <= 0.0) 0.0
    else {

      val a = Math.sqrt(Math.pow(denominatorDf, denominatorDf)) * Math.sqrt(Math.pow(numeratorDf, numeratorDf)) * Math.sqrt(Math.pow(y, numeratorDf - 2.0))
      val b = Math.sqrt(Math.pow(denominatorDf + numeratorDf * y, numeratorDf + denominatorDf)) * beta
      a / b

      /** is equivalent to
        * val a = Math.pow(numeratorDf*y, numeratorDf)*Math.pow(denominatorDf, denominatorDf)
        * val b = Math.pow(numeratorDf*y + denominatorDf, numeratorDf+denominatorDf)
        * val c = Math.sqrt(a / b)
        * val d = y * beta
        * c / d
        * */
    }
  }
}

object FDistribution {
  def apply(numeratorDf: Double, denominatorDf: Double) = new FDistribution(numeratorDf, denominatorDf)
}

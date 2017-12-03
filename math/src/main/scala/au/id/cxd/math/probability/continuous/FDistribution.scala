package au.id.cxd.math.probability.continuous

import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function.approximate.NumericIntegral
import au.id.cxd.math.function.beta.{BetaFn, IncompleteBetaFn}
import au.id.cxd.math.function.gamma.LogGammaFn
import au.id.cxd.math.function.hypergeometric.GaussHypergeometric

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
  * CDF:
  * $$
  * F(x; d_1, d_2) = I_{\frac{d_1x}{d_1x + d_2x}} \left(\frac{d_1}{2},\frac{d_2}{2}\right)
  * $$
  *
  * Where $I_{\frac{d_1x}{d_1x + d_2x}}$ is the regularized incomplete beta function.
  *
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

      /**
        *
        * f(y) = \frac{\sqrt{ (d_1 y)^{d_1} d_2^{d_2}} {\sqrt{(d_1 x + d_2)&#94;{d_1 + d_2}} \frac{1}{y Beta(\frac{d_1}{2}, \frac{d_2}{2})
        *
        */

       val a = Math.pow(numeratorDf*y, numeratorDf)*Math.pow(denominatorDf, denominatorDf)
       val b = Math.pow(numeratorDf*y + denominatorDf, numeratorDf+denominatorDf)
       val c = Math.sqrt(a / b)
       val d = y * beta
       c / d
    }
  }

  /**
    *  * CDF:
    * $$
    * F(x; d_1, d_2) = I_{\frac{d_1x}{d_1x + d_2x}} \left(\frac{d_1}{2},\frac{d_2}{2}\right)
    * $$
    *
    * Where $I_{\frac{d_1x}{d_1x + d_2x}}$ is the regularized incomplete beta function.
    *
    * @param y
    */
  def cdf(y:Double) = {
    val alpha = numeratorDf/2.0
    val beta = denominatorDf/2.0
    val x = numeratorDf*y / (numeratorDf*y + denominatorDf)

    /**
      * Method 1.
      *
      * http://mathworld.wolfram.com/F-Distribution.html
       indicates the incomplete beta function ca be defined as
      I(\frac{nx}{m+nx};n/2,m/2) =
      2n&#94;{(n-2)/2}(x/m)&#94;{n/2}\times \frac{F_((m+n)/2,1/2n,1+1/2n,-nx/m) }{B(n/2,m/2}

      Where B(a,b) is the beta function
      F_1(a,b,c,z) is a hypergeometric function
      */
    /*
    val c1 = 2*Math.pow(numeratorDf, (numeratorDf-2)/2.0) * Math.pow(y/denominatorDf, numeratorDf/2.0)
    val a = 0.5 * (numeratorDf+denominatorDf)
    val b = 0.5*numeratorDf
    val c = 1.0 + 0.5*numeratorDf
    val z = -numeratorDf*y / denominatorDf
    val f1 = GaussHypergeometric(a,b,c,z)
    val b1 = BetaFn(0.5*numeratorDf)(0.5*denominatorDf)
    val result = f1._1 / b1
    result
    */



    /** Method 2.
      *
      * note that wikipedia lists the definition of the non-regularized incomplete beta function as
      * BetaDist(x,a,b) * Exp(GammaLn(a) + GammaLn(b) - GammaLn(a+b))
      * https://en.wikipedia.org/wiki/Beta_function#Incomplete_beta_function
      *
      * Note this method gives the same result as method 1.
      */
    val incompleteBetaFn = Beta(alpha,beta).pdf(x) * Math.exp(LogGammaFn(alpha)._1 + LogGammaFn(beta)._1 - LogGammaFn(alpha+beta)._1)
    incompleteBetaFn/BetaFn(alpha)(beta)

    /**
      * method 3.
      * apply the incomplete beta function derived from the GSL.
      * This is currently tending toward 0 too quickly need further debugging to test out the
      * method.
      */
    //IncompleteBetaFn(x,alpha, beta)/BetaFn(alpha)(beta)
  }
}

object FDistribution {
  def apply(numeratorDf: Double, denominatorDf: Double) = new FDistribution(numeratorDf, denominatorDf)
}

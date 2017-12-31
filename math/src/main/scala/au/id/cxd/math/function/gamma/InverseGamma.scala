package au.id.cxd.math.function.gamma

import au.id.cxd.math.probability.continuous.Normal
import breeze.numerics.gammq

import scala.annotation.tailrec
import scala.math.{exp, pow}


/**
  * The inverse gamma function.
  *
  * This is replicated from the gammainv.c implementation in the GSL
  *
  * gsl_cdf_gamma_Pinv line 30,
  */
class InverseGamma {

  /**
    * pdf function
    * equivalent to "dgamma" in R
    *
    * @param y
    * @return
    */
  private def gammapdf(y: Double, alpha: Double, beta: Double): Double = {
    val gamma = GammaFn(alpha)
    (pow(beta, alpha) / gamma) * pow(y, alpha - 1.0) * exp(-beta * y)
    //(pow(y, a - 1.0) * exp(-y / b)) / (pow(b, a) * gamma)
  }

  /**
    * * $$
    * P( < y) = \frac{1}{\Gamma(\alpha)}\gamma(\alpha, \beta y)
    * $$
    *
    * Where $Gamma(\alpha)$ is the upper complete gamma function and $\gamma(\alpha, \beta y)$ is the lower incomplete gamma function.``
    *
    * @param y
    */
  private def gammacdf(y: Double, alpha: Double, beta: Double) = {
    val y1 = y / beta
    if (y1 <= 0.0) 0.0
    else if (y1 > alpha) 1 - IncompleteGamma.Q(alpha,y1)
    else IncompleteGamma.P(alpha,y1)
  }

  /**
    * approximate function for the inverse cdf of the standard normal distribution.
    * @param q
    */
  private def cdf_uguassian_Qinv(q:Double) = {

  }

  /**
    * Implementation from gsl_cdf_gamma_Pinv line 70
    *
    * @param x
    * @param p
    * @param a
    * @param b
    * @return
    */
  private def lagrangeInterpolate(x: Double, p: Double, a: Double, b: Double, fnDP:Double => Double, fnPhi:Double => Double): Double = {

    @tailrec def inner(n: Double, x: Double): Double = {
      val dP = fnDP(x)
      val phi = fnPhi(x)
      if (dP == 0.0 || n + 1 > 32) x
      else {
        val lambda = dP / List[Double](2 * Math.abs(dP / x), phi).max
        val step0 = lambda
        val step1 = -((a - 1.0) / x - 1) * lambda * lambda / 4.0
        val step = if (Math.abs(step1) < 0.5 * Math.abs(step0)) step0 + step1
        else step0
        val x1 = if (x + step > 0) x + step
        else x / 2.0
        if (Math.abs(step0) > 1e-10 * x || Math.abs(step0 * phi) > 1e-10 * p)
          inner(n + 1, x1)
        else x1
      }
    }

    val x1 = inner(0.0, x)
    b * x1
  }

  /**
    * gamma p inverse
    * @param p
    * @param a
    * @param b
    */
  def gammaPinv(p: Double, a: Double, b: Double) = {

    def dP(x:Double) = p - gammacdf(x, a, 1.0)

    def phi(x:Double) = gammapdf(x, a, 1.0)

    if (p == 1.0) Double.MaxValue
    else if (p == 0.0) 0.0
    else if (p < 0.05) {
      val x = Math.exp((LogGammaFn(a)._1 + Math.log(p)) / a)
      lagrangeInterpolate(x, p, a, b, dP, phi)
    } else if (p > 0.95) {
      val x = -Math.log(1 - p) + LogGammaFn(a)._1
      lagrangeInterpolate(x, p, a, b, dP, phi)
    } else {
      // we take gsl_cdf_ugaussian_Pinv to be the lower invcdf of the standard normal distribution
      val norm = Normal(0.0)(1.0)
      val xg = norm.invcdf(p)
      val x = if (xg < -0.5 * Math.sqrt(a)) a
      else Math.sqrt(a) * xg + a
      lagrangeInterpolate(x, p, a, b, dP, phi)
    }
  }

  def gammaQinv(p:Double, a:Double, b:Double) = {



  }

}

object InverseGamma {
  def apply(p: Double, a: Double, b: Double) =
    new InverseGamma().gammaPinv(p, a, b)

  def P(p:Double, a:Double, b:Double) =
    new InverseGamma().gammaPinv(p,a,b)

  def Q(p:Double, a:Double, b:Double) =
    new InverseGamma().gammaQinv(p,a,b)
}

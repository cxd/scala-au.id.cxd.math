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
    * copy of gsl_ran_gamma_pdf line 131 randist/gamma.c
    * @param x
    * @param a
    * @param b
    * @return
    */
  private def ran_gammapdf(x:Double, a:Double, b:Double) :Double = {
    if (x < 0) 0.0
    else if (x == 0.0) {
      if (a == 1.0) 1.0/b
      else 0.0
    } else if (a == 1.0) {
      Math.exp(-x/b)/b
    } else {
      val lng = LogGammaFn(a)._1
      val p = Math.exp((a-1.0) * Math.log(x/b) - x/b - lng)/b
      p
    }
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

  private def gammacdfQ(y:Double, a:Double, b:Double) = {
    val y1 = y/b
    if (y <= 0) 1.0
    else if (y1 < a) 1 - IncompleteGamma.P(a, y1)
    else IncompleteGamma.Q(a,y1)
  }


  private def ratEval (x:Double, a:List[Double], b:List[Double]) = {
    var u = a(a.length - 1)
    for (i <- a.length - 1 to 1 by -1) yield {
      u = x * u + a(i-1)
    }
    var v = b(b.length - 1)
    for (j <- b.length - 1 to 1 by -1) yield {
      v = x*v + b(j-1)
    }
    val r = u/v
    r
  }

  private def smallrat_eval(q:Double) = {
    val a = List[Double](
      3.387132872796366608, 133.14166789178437745,
      1971.5909503065514427, 13731.693765509461125,
      45921.953931549871457, 67265.770927008700853,
      33430.575583588128105, 2509.0809287301226727
    )
    val b = List[Double](
      1.0, 42.313330701600911252,
      687.1870074920579083, 5394.1960214247511077,
      21213.794301586595867, 39307.89580009271061,
      28729.085735721942674, 5226.495278852854561
    )
    val r = 0.180625 - q*q
    val x = q * ratEval(r, a, b)
    x
  }

  private def intermediate_rat_eval(q:Double) = {
    val a = List[Double](
      1.42343711074968357734, 4.6303378461565452959,
      5.7694972214606914055, 3.64784832476320460504,
      1.27045825245236838258, 0.24178072517745061177,
      0.0227238449892691845833, 7.7454501427834140764e-4
    )
    val b = List[Double](
      1.0, 2.05319162663775882187,
      1.6763848301838038494, 0.68976733498510000455,
      0.14810397642748007459, 0.0151986665636164571966,
      5.475938084995344946e-4, 1.05075007164441684324e-9
    )
    val x = ratEval((q-1.6), a, b)
    x
  }

  private def tail_rat_eval(q:Double) = {
    val a = List[Double](
      6.6579046435011037772, 5.4637849111641143699,
      1.7848265399172913358, 0.29656057182850489123,
      0.026532189526576123093, 0.0012426609473880784386,
      2.71155556874348757815e-5, 2.01033439929228813265e-7
    )
    val b = List[Double](
      1.0, 0.59983220655588793769,
      0.13692988092273580531, 0.0148753612908506148525,
      7.868691311456132591e-4, 1.8463183175100546818e-5,
      1.4215117583164458887e-7, 2.04426310338993978564e-15
    )
    val x = ratEval((q-5.0), a, b)
    x
  }

  /**
    * approximate function for the inverse cdf of the standard normal distribution.
    * implementation from the GSL gaussinv.c line 146
    * @param q
    */
  private def cdf_uguassian_Qinv(q:Double) = {
    val dp = q - 0.5
    if (q == 1.0) Double.NegativeInfinity
    else if (q == 0.0) Double.PositiveInfinity
    else if (Math.abs(dp) <= 0.425) {
      val x = smallrat_eval(dp)
      -x
    } else {
      val pp = if (q < 0.5) q else 1.0 - q
      val r = Math.sqrt(-Math.log(pp))
      val x = if (r <= 5.0) intermediate_rat_eval(r)
      else tail_rat_eval(r)
      if (q < 0.5) x
      else -x
    }
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

    if (p == 1.0) Double.PositiveInfinity
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

  /**
    * implementation of gsl_cdf_gamma_Qinv from gammainv.c line 119
    * @param q
    * @param a
    * @param b
    * @return
    */
  def gammaQinv(q:Double, a:Double, b:Double) = {

    def dQ (x:Double) = -(q - gammacdfQ(x,a,1.0))
    def dPhi (x:Double) = gammapdf(x,a,1.0)

    if (q == 1.0) 0.0
    else if (q == 0.0) Double.PositiveInfinity
    else if (q < 0.05) {
      val x = -Math.log(q) + LogGammaFn(a)._1
      lagrangeInterpolate(x,q,a,b, dQ, dPhi)
    } else if (q > 0.95) {
      val x = Math.exp( (LogGammaFn(a)._1 + Math.log(1.0-q) ) / a)
      lagrangeInterpolate(x,q,a,b, dQ, dPhi)
    } else {
      val xg = cdf_uguassian_Qinv(q)
      val x0 = if (xg < -0.5 * Math.sqrt(a)) a
      else Math.sqrt(a)*xg + a
      lagrangeInterpolate(x0, q, a, b, dQ, dPhi)
    }
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

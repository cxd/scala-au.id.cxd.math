package au.id.cxd.math.function.gamma

import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function.Constants
import au.id.cxd.math.function.series.ContinuedSeries

/**
  * Log gamma function implementation.
  *
  * derived from the GSL implementation and the Apache Commons Numbers implementation.
  *
  * Refer to GSL gamma.c
  */
class LogGammaFn extends ContinuedSeries {
  /* coefficients for gamma=7, kmax=8  Lanczos method */
  private val lanczcos7 = List(
    0.99999999999980993227684700473478,
    676.520368121885098567009190444019,
    -1259.13921672240287047156078755283,
    771.3234287776530788486528258894,
    -176.61502916214059906584551354,
    12.507343278686904814458936853,
    -0.13857109526572011689554707,
    9.984369578019570859563e-6,
    1.50563273514931155834e-7)


  /**
    * compute the pade approximation for ln gamma using supplied coefficients
    * There are two use cases where abs(x-1) < 0.01 and abs(x-2) < 0.02
    *
    * @param n
    * @param d
    * @param padeCoeff
    * @param coeffs
    * @param eps
    * @return
    */
  private def lnGammaPade(n: (Double, Double), d: (Double, Double), padeCoeff: Double, coeffs: List[Double])(eps: Double):(Double,Double) = {
    val num = (eps + n._1) * (eps + n._2)
    val den = (eps + d._1) * (eps + d._2)
    val pade = padeCoeff * num / den
    val eps5 = Math.pow(eps, 5)
    val corr = eps5 * (pade + additiveProductFn(coeffs, eps))
    val err = 2.0 * Constants.DBL_EPSILON * Math.abs(corr)
    (corr, err)
  }

  /**
    * Use (2,2) Pade for Log[Gamma[1+eps]]/eps
    * plus a correction series.
    *
    * See GSL gamma.c line 897
    */
  private def lnGammaPade1(eps: Double):(Double,Double) = {
    val n = (-1.0017419282349508699871138440, 1.7364839209922879823280541733)
    val d = (1.2433006018858751556055436011, 5.0456274100274010152489597514)
    val padeCoeff = 2.0816265188662692474880210318
    val coeffs = List(0.004785324257581753,
      -0.01192457083645441,
      0.01931961413960498,
      -0.02594027398725020,
      0.03141928755021455)
    lnGammaPade(n, d, padeCoeff, coeffs)(eps)
  }

  /**
    * Use (2,2) Pade for Log[Gamma[2+eps]]/eps
    * plus a correction series.
    * See GSL gamma.c line 924
    */
  private def lnGammaPade2(eps: Double):(Double,Double) = {
    val n = (1.000895834786669227164446568, 4.209376735287755081642901277)
    val d = (2.618851904903217274682578255, 10.85766559900983515322922936)
    val padeCoeff = 2.85337998765781918463568869
    val coeffs = List(0.0001139406357036744,
      -0.0001365435269792533,
      0.0001067287169183665,
      -0.0000693271800931282,
      0.0000407220927867950
    )
    lnGammaPade(n, d, padeCoeff, coeffs)(eps)
  }


  /**
    * Lanczos method for real x > 0;
    * gamma=7, truncated at 1/(z+8)
    * [J. SIAM Numer. Anal, Ser. B, 1 (1964) 86]
    *
    * Refer to GSL gamma.c line 697
    */
  private def lnGammaLanczos(x: Double):(Double,Double) = {
    val x1 = x - 1.0
    val kPairs = lanczcos7.zipWithIndex.tail
    val Ag = lanczcos7.head + kPairs.map {
      pair => pair._1 / (x1 + pair._2)
    }.reduce(_ + _)
    val terms = (
      (x1 + 0.5) * Math.log((x1 + 7.5) / Math.E),
      Constants.LOG_ROOT_TWO_PI + Math.log(Ag)
    )
    val y = terms._1 + (terms._2 - 7.0)
    val err1 = 2.0 * Constants.DBL_EPSILON * (Math.abs(terms._1) + Math.abs(terms._2) + 7.0)
    val err2 = Constants.DBL_EPSILON * Math.abs(y)
    (y, err1 + err2)
  }


  /** x = eps near zero
    * gives double-precision for |eps| < 0.02
    * B calculate series for g(eps) = Gamma(eps) eps - 1/(1+eps) - eps/2
    *
    * See GSL gamma.c Line 731
    * */
  private def sign0(eps: Double):(Double,Double) = {
    val coeffs = List(
      -0.07721566490153286061,
      -0.01094400467202744461,
      0.09252092391911371098,
      -0.01827191316559981266,
      0.01800493109685479790,
      -0.00685088537872380685,
      0.00399823955756846603,
      -0.00189430621687107802,
      0.00097473237804513221,
      -0.00048434392722255893
    )
    val g = eps * additiveProductFn(coeffs, eps)
    /* calculate Gamma(eps) eps, a positive quantity */
    val g1 = g + 1.0 / (1.0 + eps) + 0.5 * eps
    val y = Math.log(g1 / Math.abs(eps))
    val err = 4.0 * Constants.DBL_EPSILON * Math.abs(y)
    (y, err)
  }


  /** x near a negative integer
    * Calculates sign as well as log(|gamma(x)|).
    * x = -N + eps
    * assumes N >= 1
    *
    * See GSL gamma.c line 763
    */
  private def sinGamma(N: Double, eps: Double):(Double,Double) = {
    if (N == 1) {
      /** calculate series for
        * g = eps gamma(-1+eps) + 1 + eps/2 (1+3eps)/(1-eps^2)
        * double-precision for |eps| < 0.02
        * */
      val coeffs = List(
        0.07721566490153286061,
        0.08815966957356030521,
        -0.00436125434555340577,
        0.01391065882004640689,
        -0.00409427227680839100,
        0.00275661310191541584,
        -0.00124162645565305019,
        0.00065267976121802783,
        -0.00032205261682710437,
        0.00016229131039545456
      )
      val g = eps * additiveProductFn(coeffs, eps)
      /* calculate eps gamma(-1+eps), a negative quantity */
      val gam_e = g - 1.0 - 0.5 * eps * (1.0 + 3.0 * eps) / (1.0 - eps * eps)
      val y = Math.log(Math.abs(gam_e) / Math.abs(eps))
      val err = 2.0 * Constants.DBL_EPSILON * Math.abs(y)
      (y, err)
    } else {
      /** series for sin(Pi(N+1-eps))/(Pi eps) modulo the sign
        * double-precision for |eps| < 0.02
        */
      val coeffs1 = List(
        -1.6449340668482264365,
        0.8117424252833536436,
        -0.1907518241220842137,
        0.0261478478176548005,
        -0.0023460810354558236
      )
      val e2 = eps * eps
      val sin_ser = 1.0 + e2 * additiveProductFn(coeffs1, e2)

      /** calculate series for ln(gamma(1+N-eps))
        * double-precision for |eps| < 0.02
        */
      val logFn = (n: Double) => logGamma(n + 1)
      val aeps = Math.abs(eps)
      val (c0, err0) = lnFact(N)
      val (r1, err1) = Digamma(N + 1)
      val (r2, err2) = Trigamma(logFn, N + 1)
      val (r3:Double, err3) = if (aeps > 0.00001) Polygamma(logFn, 2, N+1)
      else 0.0
      val (r4:Double,err4) = if (aeps > 0.0002) Polygamma(logFn, 3, N+1)
      else 0.0
      val (r5:Double, err5) = if (aeps > 0.001) Polygamma(logFn, 4, N+1)
      else 0.0
      val (r6:Double, err6) = if (aeps > 0.005) Polygamma(logFn, 5, N+1)
      else 0.0
      val (r7:Double, err7) = if (aeps > 0.01) Polygamma(logFn, 6, N+1)
      else 0.0
      val coeffs2 = List(
        r1,
        r2/2.0,
        r3/6.0,
        r4/24.0,
        r5/120.0,
        r6/720.0,
        r7/5040.0
      )
      val lng = c0 - eps * additiveProductFn(coeffs2, eps)
      val g = lng - Math.log(sin_ser)
      val y = g - Math.log(Math.abs(eps))
      val err = c0 + 2.0*Constants.DBL_EPSILON*(Math.abs(g) + Math.abs(y))
      (y,err)
    }
  }

  /**
    * compute the logarithm of the factorial.
    *
    * @param n
    * @return
    */
  private def lnFact(n: Double):(Double,Double) = {
    if (n <= Factorial.MAX_N) {
      val y = Math.log(Factorial(n))
      val err = 2.0 * Constants.DBL_EPSILON * Math.abs(y)
      (y, err)
    } else logGamma(n + 1)
  }

  /**
    * Compute the logarithm of the gamma function
    * given that x is not negative and not 0.
    * If x < 0 the real part of loggamma is returned.
    * The implementation is a copy of the GSL implementation
    *
    * see GSL gamma.c l_sf_lngamma_e line: 1114
    *
    * @param x
    * @return
    */
  private def logGamma(x: Double):(Double, Double) = if (Math.abs(x - 1.0) < 0.01) {
    val (y, err) = lnGammaPade1(x - 1.0)
    val err1 = 1.0 / (Constants.DBL_EPSILON + Math.abs(x - 1.0))
    (y, err1)
  } else if (Math.abs(x - 2.0) < 0.01) {
    val (y, err) = lnGammaPade2(x - 2.0)
    val err1 = 1.0 / (Constants.DBL_EPSILON + Math.abs(x - 2.0))
    (y, err1)
  } else if (x >= 0.5) {
    val (y, err) = lnGammaLanczos(x)
    (y, err)
  } else if (x == 0.0) {
    // error case
    throw new IllegalArgumentException(s"x == 0.0 invalid argument for log gamma")
  } else if (Math.abs(x) < 0.02) {
    val (y, err) = sign0(x)
    (y, err)
  } else if (x > -0.5 / (Constants.DBL_EPSILON * Math.PI)) {
    /**
      * extract a fractional part of x.
      */
    val z = 1.0 - x
    val s = Math.sin(Math.PI * z)
    val as = Math.abs(s)
    if (s == 0) {
      throw new IllegalArgumentException(s"x $x has no fractional part in log gamma")
    }
    else if (as < Math.PI * 0.015) {
      /* x is near a negative integer, -N */
      if (x < Int.MinValue + 2.0) {
        (0.0, 0.0)
      } else {
        val N = -(x - 0.5).toInt.toDouble
        val eps = x + N
        if (eps == 0) {
          (0.0, 0.0)
        } else {
          sinGamma(N, eps)
        }
      }
    } else {
      val (y, err) = lnGammaLanczos(z)
      val y1 = Constants.LN_PI - (Math.log(as) + y)
      val err1 = 2.0 * Constants.DBL_EPSILON + Math.abs(y) + err
      (y1, err1)
    }
  } else {
    /* |x| was too large to extract any fractional part */
    (0.0, 0.0)
  }


  def opt(x: Double) = logGamma(x)

}

object LogGammaFn {
  def apply(x: Double) = new LogGammaFn().opt(x)
}

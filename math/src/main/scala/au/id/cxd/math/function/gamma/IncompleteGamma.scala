package au.id.cxd.math.function.gamma

import au.id.cxd.math.function.Constants
import au.id.cxd.math.function.approximate.ContinuedFractionLentz
import au.id.cxd.math.function.exponential.ExpRel
import au.id.cxd.math.function.series.ContinuedSeries

/**
  * implementation of incomplete gamma functions.
  *
  * This is used in the gamma distributions and in the inverse gamma functions.
  *
  * Replicated directly from GSL special/gamma_inc.c
  *
  *
  */
class IncompleteGamma extends ContinuedSeries {

  /**
    * implementation from gamma_inc.c
    * gamma_inc_D line 39
    *
    * @param a
    * @param x
    */
  private def gamma_inc_D(a: Double, x: Double) = {
    if (a < 10.0) {
      val temp = LogGammaFn(a + 1.0)._1
      val lnr = a * Math.log(x) - x - temp
      Math.exp(lnr)
    } else {
      val ln_term = if (x < 0.5 * a) {
        val u = x / a
        val ln_u = Math.log(u)
        ln_u - u + 1.0
      } else {
        val mu = (x - a) / a
        Math.log(1 + mu) - mu
      }
      val gstar = GammaStarFn(a)
      val term1 = Math.exp(a * ln_term) / Math.sqrt(2.0 * Math.PI * a)
      term1 / gstar
    }
  }

  /**
    * Implementation of gamma_inc_F_CF from gamma_inc.c line 247
    *
    * Continued fraction which occurs in evaluation
    * of Q(a,x) or Gamma(a,x).
    *
    * 1   (1-a)/x  1/x  (2-a)/x   2/x  (3-a)/x
    * F(a,x) =  ---- ------- ----- -------- ----- -------- ...
    * 1 +   1 +     1 +   1 +      1 +   1 +
    *
    * Hans E. Plesser, 2002-01-22 (hans dot plesser at itf dot nlh dot no).
    *
    * Split out from gamma_inc_Q_CF() by GJ [Tue Apr  1 13:16:41 MST 2003].
    * See gamma_inc_Q_CF() below.
    *
    */
  private def gamma_inc_F_CF(a: Double, x: Double) = {
    val nmax = 5000
    val small = Constants.DBL_EPSILON * Constants.DBL_EPSILON * Constants.DBL_EPSILON

    def isOdd(i: Double) = if (i % 2.0 == 0.0) false
    else true

    def aFn(n: Double, x: Double, aj: Double, bj: Double) = {
      if (isOdd(n))
        0.5 * (n - 1.0) / x
      else (0.5 * n - a) / x
    }

    // bn defaults to 1.0 in this function
    def bFn(n: Double, x: Double, aj: Double, bj: Double) = 1.0
    // calculate continued fraction with initial values.
    ContinuedFractionLentz(x, 1.0 / small, 1.0, 1.0, aFn, bFn, nmax)
  }

  /**
    * implement gamma_inc_p_series from gamma_inc.c line 83
    *
    * @param a
    * @param x
    * @return
    */
  private def gamma_inc_P_series(a: Double, x: Double) = {
    val nmax = 10000
    val d = gamma_inc_D(a, x)
    /*
    gamma_inc.c line 90
    Approximating the terms of the series using Stirling's
     approximation gives t_n = (x/a)^n * exp(-n(n+1)/(2a)), so the
     convergence condition is n^2 / (2a) + (1-(x/a) + (1/2a)) n >>
     -log(GSL_DBL_EPS) if we want t_n < O(1e-16) t_0. The condition
     below detects cases where the minimum value of n is > 5000 */
    if (x > 0.995 * a && a > 1e5) {
      val cf = ExpRel(a, x)
      d * cf
    } else if (x > (a + nmax)) {
      throw new IllegalArgumentException(s"x is too large cannot estimate gammaP $x")
    } else {
      /* Handle lower part of the series where t_n is increasing, |x| > a+n */
      val nlow = if (x > a) {
        x - a
      } else 0.0
      val result1 = (for (i <- 1 until nlow.toInt) yield i).foldLeft((1.0, 1.0, 1.0)) {
        (accum, i) => {
          val term = (x / (a + i)) * accum._2
          (i, term, accum._3 + term)
        }
      }
      val n1 = if (nlow <= 1.0) 1.0
      else result1._1 + 1.0
      val term = result1._2
      val sum = result1._3

      val result2 = (for (j <- n1.toInt until nmax) yield j).foldLeft((n1, term, sum, false)) {
        (accum, j) => {
          if (accum._4) accum
          else {
            val term = (x / (a + j)) * accum._2
            val sum = accum._3 + term
            if (Math.abs(term / sum) < Constants.DBL_EPSILON) (j, term, sum, true)
            else (j, term, sum, false)
          }

        }
      }
      val (n2, term2, sum2, flag) = result2

      /*  Estimate remainder of series ~ t_(n+1)/(1-x/(a+n+1)) */
      d * sum2
    }
  }

  /**
    * implementation of gamma_inc_Q_CF from gamma_inc.c line 321
    *
    * @param a
    * @param x
    * @return
    */
  private def gamma_inc_Q_CF(a: Double, x: Double) = {
    val statD = gamma_inc_D(a, x)
    val statF = gamma_inc_F_CF(a, x)
    statD * (a / x) * statF
  }


  /**
    * implementation of gamma_inc_Q_asymp_unif
    * gamma_inc.c
    * line 192
    *
    * @param a
    * @param x
    */
  private def gamma_inc_Q_asymp_unif(a: Double, x: Double) = {
    def sign(k: Double) = if (k < 0) -1.0 else 1.0

    val rta = Math.sqrt(a)
    val eps = (x - a) / a
    val ln_term = Math.log(1 + eps) - eps
    val eta = sign(eps) * Math.sqrt(-2.0 * ln_term)
    val erfc = Erfc(eta * rta / Constants.SQRT2)
    val (c0, c1) = if (Math.abs(eps) < Constants.ROOT5_DBL_EPSILON) {
      val temp0 = -1.0 / 3.0 + eps * (1.0 / 12.0 - eps * (23.0 / 540.0 - eps * (353.0 / 12960.0 - eps * 589.0 / 30240.0)))
      val temp1 = -1.0 / 540.0 - eps / 288.0
      (temp0, temp1)
    } else {
      val rt_term = Math.sqrt(-2.0 * ln_term / (eps * eps))
      val lam = x / a
      val temp0 = (1.0 - 1.0 / rt_term) / eps
      val temp1 = -(eta * eta * eta * (lam * lam + 10.0 * lam + 1.0) - 12.0 * eps * eps * eps) / (12.0 * eta * eta * eta * eps * eps * eps)
      (temp0, temp1)
    }
    val R = Math.exp(-0.5 * a * eta * eta) / (Constants.SQRT2 * Constants.SQRT_PI * rta) * (c0 + c1 / a)
    0.5 * erfc + R
  }

  /**
    * implementation of gamm_inc_Q_large_x copied directly from gamma_inc.c line 157
    *
    * @param a
    * @param x
    */
  private def gamma_inc_Q_large_x(a: Double, x: Double) = {
    val nmax = 5000
    val sum = 1.0
    val term = 1.0
    val last = 1.0
    val D = gamma_inc_D(a, x)
    val (sumA, termA, lastA, flagA) = (for (n <- 1 until nmax) yield n)
      .foldLeft((1.0, 1.0, 1.0, false)) {
        (accum, n) => {
          val (sum, term, last, flag) = accum
          if (flag) accum
          else {
            val term1 = term * (a - n) / x
            if (Math.abs(term1 / last) > 1.0) {
              (sum, term1, term1, true)
            } else if (Math.abs(term1 / sum) < Constants.DBL_EPSILON) {
              (sum, term1, term1, true)
            } else {
              (sum + term1, term1, term1, flag)
            }
          }

        }
      }
    D * (a / x) * sum
  }

  /**
    * implementation of gamma_inc_Q_series
    * from gamma_inc.c gamma_inc_Q_series line: 339
    * with slight modification.
    *
    * @param a
    * @param x
    * @return
    */
  private def gamma_inc_Q_series(a: Double, x: Double) = {
    // polygamma 2,1
    val pg21 = -2.404113806319188570799476
    val lnx = Math.log(x)
    val el = Constants.EULER + lnx
    val c1 = -el
    val c2 = Math.PI * Math.PI / 12.0 - 0.5 * el * el
    val c3 = el * (Math.PI * Math.PI / 12.0 - el * el / 6.0) + pg21 / 6.0
    val c4 = -0.04166666666666666667 *
      (-1.758243446661483480 + lnx) *
      (-0.764428657272716373 + lnx) *
      (0.723980571623507657 + lnx) *
      (4.107554191916823640 + lnx)

    val c5 = -0.0083333333333333333 *
      (-2.06563396085715900 + lnx) *
      (-1.28459889470864700 + lnx) *
      (-0.27583535756454143 + lnx) *
      (1.33677371336239618 + lnx) *
      (5.17537282427561550 + lnx)

    val c6 = -0.0013888888888888889 *
      (-2.30814336454783200 + lnx) *
      (-1.65846557706987300 + lnx) *
      (-0.88768082560020400 + lnx) *
      (0.17043847751371778 + lnx) *
      (1.92135970115863890 + lnx) *
      (6.22578557795474900 + lnx)

    val c7 = -0.00019841269841269841 *
      (-2.5078657901291800 + lnx) *
      (-1.9478900888958200 + lnx) *
      (-1.3194837322612730 + lnx) *
      (-0.5281322700249279 + lnx) *
      (0.5913834939078759 + lnx) *
      (2.4876819633378140 + lnx) *
      (7.2648160783762400 + lnx)

    val c8 = -0.00002480158730158730 *
      (-2.677341544966400 + lnx) *
      (-2.182810448271700 + lnx) *
      (-1.649350342277400 + lnx) *
        (-1.014099048290790 + lnx) *
        (-0.191366955370652 + lnx) *
      (0.995403817918724 + lnx) *
      (3.041323283529310 + lnx) *
      (8.295966556941250 + lnx)

    val c9 = -2.75573192239859e-6 *
      (-2.8243487670469080 + lnx) *
      (-2.3798494322701120 + lnx) *
      (-1.9143674728689960 + lnx) *
      (-1.3814529102920370 + lnx) *
      (-0.7294312810261694 + lnx) *
      (0.1299079285269565 + lnx) *
      (1.3873333251885240 + lnx) *
      (3.5857258865210760 + lnx) *
      (9.3214237073814600 + lnx)

    val c10 = -2.75573192239859e-7 *
      (-2.9540329644556910 + lnx) *
      (-2.5491366926991850 + lnx) *
      (-2.1348279229279880 + lnx) *
      (-1.6741881076349450 + lnx) *
      (-1.1325949616098420 + lnx) *
      (-0.4590034650618494 + lnx) *
      (0.4399352987435699 + lnx) *
      (1.7702236517651670 + lnx) *
      (4.1231539047474080 + lnx) *
      (10.342627908148680 + lnx)

    val term1 = a * poly(List(c1, c2, c3, c4, c5, c6, c7, 8, c9, 10), a)
    val nmax = 5000
    val (t1, sum1, flag1) = (for (n <- 1 to nmax) yield n)
      .foldLeft((1.0, 1.0, false)) {
        (accum, n) => {
          val (t, sum, flag) = accum
          if (flag) accum
          else {
            val t2 = t * -x / (n + 1.0)
            val sum2 = sum + (a + 1.0) / (a + n + 1.0) * t2
            if (Math.abs(t / sum) < Constants.DBL_EPSILON) {
              (t2, sum2, true)
            } else (t2, sum2, false)
          }
        }
      }
    val term2 = (1.0 - term1) * a / (a + 1.0) * x * sum1
    term1 + term2
  }

  /**
    * implementation of the complementary incomplete gamma function
    *
    * based directly on gsl_sf_gamma_inc_Q_e in gamma_inc.c line: 500
    *
    * @param x
    * @return
    */
  def gammaincQ(a: Double, x: Double) = {
    if (a < 0.0 || x < 0.0) {
      throw new IllegalArgumentException(s"parameters cannot be less than 0 a=$a x=$x")
    } else if (x == 0.0) 1.0
    else if (a == 0.0) 0.0
    else if (x <= 0.5 * a) {
      /* If the series is quick, do that. It is
     * robust and simple.
     */
      val P = gamma_inc_P_series(a, x)
      1.0 - P
    } else if (a >= 1.0e+06 && (x - a) * (x - a) < a) {
      /* Then try the difficult asymptotic regime.
     * This is the only way to do this region.
     */
      gamma_inc_Q_asymp_unif(a, x)
    } else if (a < 0.02 && x < 5.0) {
      /* Cancellations at small a must be handled
     * analytically; x should not be too big
     * either since the series terms grow
     * with x and log(x).
     */
      gamma_inc_Q_series(a, x)
    } else if (a <= x) {
      if (x <= 1.0e+06) {
        /* Continued fraction is excellent for x >~ a.
       * We do not let x be too large when x > a since
       * it is somewhat pointless to try this there;
       * the function is rapidly decreasing for
       * x large and x > a, and it will just
       * underflow in that region anyway. We
       * catch that case in the standard
       * large-x method.
       */
        gamma_inc_Q_CF(a, x)
      } else gamma_inc_Q_large_x(a, x)
    } else {
      if (x < a - Math.sqrt(a)) {
        /* Continued fraction again. The convergence
       * is a little slower here, but that is fine.
       * We have to trade that off against the slow
       * convergence of the series, which is the
       * only other option.
       */
        gamma_inc_Q_CF(a, x)
      } else {
        1.0 - gamma_inc_P_series(a, x)
      }
    }
  }

  /**
    * implementation of incomplete gamma P from
    * gsl_sf_gamma_inc_P_e gamma_inc.c line: 581
    *
    * @param a
    * @param x
    * @return
    */
  def gammaincP(a: Double, x: Double) = {
    if (a <= 0.0 || x < 0.0) {
      throw new IllegalArgumentException(s"parameters cannot be less than 0 a=$a x=$x")
    }
    else if (x == 0.0) 0.0
    else if (x < 20.0 || x < 0.5 * a) {
      gamma_inc_P_series(a, x)
    } else if (a > 1.0e+06 && (x - a) * (x - a) < a) {
      /* Crossover region. Note that Q and P are
     * roughly the same order of magnitude here,
     * so the subtraction is stable.
     */
      val Q = gamma_inc_Q_asymp_unif(a, x)
      1.0 - Q
    } else if (a <= x) {
      /* Q <~ P in this area, so the
     * subtractions are stable.
     */
      val Q = if (a > 0.2 * x) {
        gamma_inc_Q_CF(a, x)
      } else {
        gamma_inc_Q_large_x(a, x)
      }
      1.0 - Q
    } else if ((x - a) * (x - a) < a) {
      /* This condition is meant to insure
             * that Q is not very close to 1,
             * so the subtraction is stable.
             */
      val Q = gamma_inc_Q_CF(a, x)
      1.0 - Q
    } else {
      gamma_inc_P_series(a, x)
    }
  }

}

object IncompleteGamma {

  def apply() = new IncompleteGamma()

  def apply(a: Double, x: Double, complement: Boolean = false) = if (complement)
    new IncompleteGamma().gammaincQ(a, x)
  else new IncompleteGamma().gammaincP(a, x)

  def P(a: Double, x: Double) = new IncompleteGamma().gammaincP(a, x)

  def Q(a: Double, x: Double) = new IncompleteGamma().gammaincQ(a, x)

}
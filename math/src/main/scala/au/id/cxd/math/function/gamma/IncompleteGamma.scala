package au.id.cxd.math.function.gamma

import au.id.cxd.math.function.exponential.ExpRel

/**
  * implementation of incomplete gamma functions.
  *
  * This is used in the gamma distributions and in the inverse gamma functions.
  *
  * Replicated from GSL special/gamma_inc.c
  *
  *
  */
class IncompleteGamma {

  /**
    * implementation from gamma_inc.c
    * gamma_inc_D line 39
    *
    * @param a
    * @param x
    */
  def gamma_inc_D(a:Double, x:Double) = {
    if (a < 10.0) {
      val temp = LogGammaFn(a+1.0)._1
      val lnr = a * Math.log(x) - x - temp
      Math.exp(lnr)
    } else {
      val ln_term = if (x < 0.5*a) {
        val u = x/a
        val ln_u = Math.log(u)
        ln_u - u + 1.0
      } else {
        val mu = (x - a)/a
        Math.log(1+mu) - mu
      }
      val gstar = GammaStarFn(a)
      val term1 = Math.exp(a*ln_term) / Math.sqrt(2.0*Math.PI*a)
      term1 / gstar
    }
  }

  /**
    * implement gamma_inc_p_series from gamma_inc.c line 83
    * @param a
    * @param x
    * @return
    */
  def gamma_inc_P_series(a: Double, x: Double) = {
    val nmax = 10000
    val d = gamma_inc_D(a,x)
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
    } else if (x > (a + nmax) ) {
      throw new IllegalArgumentException(s"x is too large cannot estimate gammaP $x")
    } else {
      /* Handle lower part of the series where t_n is increasing, |x| > a+n */
      val nlow = if (x > a) { x - a } else 0.0
      val result1 = (for (i <- 1 to nlow.toInt) yield i).foldLeft((0.0, 1.0, 0.0)) {
          (accum, i) => {
            val term = (x/(a+i))*accum._2
            (i, term, accum._3 + term)
          }
        }
      val n1 = result1._1
      val term = result1._2
      val sum = result1._3
      /*  Estimate remainder of series ~ t_(n+1)/(1-x/(a+n+1)) */
      d * sum
    }
  }

  def gammaincQ(a: Double, x: Double) = {
    0.0
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
      gamma_inc_P_series(a,x)
    } else if (a > 1.0e+06 && (x - a) * (x - a) < a) {
      /* Crossover region. Note that Q and P are
     * roughly the same order of magnitude here,
     * so the subtraction is stable.
     */
      // TODO: gamma_inc_Q_asymp_unif
      0.0
    } else if (a <= x) {
      /* Q <~ P in this area, so the
     * subtractions are stable.
     */
      // TODO: gamma_inc_Q_CF
      // TODO: gamma_inc_Q_large_x
      0.0
    } else if ((x - a) * (x - a) < a) {
      /* This condition is meant to insure
             * that Q is not very close to 1,
             * so the subtraction is stable.
             */
      // TODO: gamma_inc_Q_CF
      0.0
    } else {
      gamma_inc_P_series(a,x)
    }
  }

}

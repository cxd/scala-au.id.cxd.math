package au.id.cxd.math.function.beta

import au.id.cxd.math.function.Constants
import au.id.cxd.math.probability.continuous.Beta
import au.id.cxd.math.function.hypergeometric.GaussHypergeometric

import scala.annotation.tailrec

/**
  * ##import MathJax
  *
  * CDF =
  * $$
  * I_x (\alpha, \beta) = \frac{B(x; \alpha,\beta)}{B(\alpha,\beta)}
  * $$
  * The regularised incomplete beta function
  *
  * If x = 0, $I_x$ = 0, x=1, $I_x$ = 1, if $\alpha = 1$, $I_x = 1 - (1-x)&#94;\beta$.
  * If $\beta = 1$ $I_x = x&#94;\alpha$.
  *
  * Note that the complete beta function can be approximated by
  * $$
  * \Gamma(x)\Gamma(y) = B(x,y)\Gamma(x + y)
  * $$
  * $$
  * B(x,y) = \frac{\Gamma(x)\Gamma(y)}{\Gamma(x + y)}
  * $$
  *
  * The incomplete beta function can be written the product of the beta distribution and the complete gamma function
  *
  * https://en.wikipedia.org/wiki/Beta_function#Properties
  */
class IncompleteBetaFn {

  /**
    * an implementation of the continued fraction calculation for beta
    * as copied from the GSL beta_inc.c line 42
    * @param a
    * @param b
    * @param x
    */
  private def beta_cont_frac(a:Double, b:Double, x:Double) = {
    val max_iter = 512
    val cutoff = 2.0* Constants.DBL_MIN
    val num_term = 1.0
    val temp = 1.0 - (a+b)*x/(a+1.0)
    val den_term = if (Math.abs(temp) < cutoff) 1.0/cutoff
                   else 1.0/temp

    val cf = den_term
    // note this recursion seems to tend towards 0 very rapidly
    @tailrec def continue (iter_count:Int, num_term1:Double, den_term1:Double, cf1:Double):Double = {
      val k = (iter_count+1).toDouble
      val coeff = k * (b-k)*x / ( ((a-1.0)+2.0*k)* (a+2.0*k))

      val temp_den2 = 1.0 + coeff*den_term1
      val temp_num2 = 1.0 * coeff/num_term1

      val den_term2 = if (Math.abs(temp_den2) < cutoff) 1.0/cutoff
      else 1.0/temp_den2

      val num_term2 = if (Math.abs(temp_num2) < cutoff) cutoff
      else temp_num2

      val delta_frac1 = den_term2 * num_term2
      val cf2 = cf1 * delta_frac1

      val coeff2 = -(a+k)*(a+b+k)*x  / ( (a+2.0*k)*(a+2*k+1.0))

      val temp_den3 = 1.0 + coeff2 * den_term2
      val temp_num3 = 1.0 + coeff2 / num_term2

      val den_term3 = if (Math.abs(temp_den3) < cutoff) 1.0/cutoff
      else 1.0/temp_den3

      val num_term3 = if (Math.abs(temp_num3) < cutoff) cutoff
      else temp_num3

      val delta_frac2 = den_term3 * num_term3
      val cf3 = cf2 * delta_frac2


      if (Math.abs(delta_frac2-1.0) < 2.0 * Constants.DBL_EPSILON) cf3
      else if (iter_count+1 >= max_iter) cf3
      else continue (iter_count+1, num_term3, den_term3, cf3)
    }

    continue (0, num_term, den_term, cf)
  }

  /**
    * This implementation of the incomplete beta function is
    * a copy of the GSL implementation in beta_inc.c line 107
    * @param x
    * @param a
    * @param b
    */
  private def incompleteBeta(x:Double, a:Double, b:Double):Double = {
    def isNeg(z:Double)= z < 0.0 && z == Math.floor(z)

    if (x < 0.0 || x > 1.0) {
      throw new IllegalArgumentException(s"x = $x is out of range cannot be less than 0 or greater than 1")
    }
    if (isNeg(a) || isNeg(b)) {
      throw new IllegalArgumentException(s"alpha ($a) is a negative whole number || beta ($b) is a negative whole number")
    }
    if (isNeg(a+b)) {
      throw new IllegalArgumentException(s"(alpha + beta) is a negative whole number")
    }
    if (x == 0.0) {
      0.0
    } else if (x == 1.0) {
      1.0
    } else if (a <= 0 || b <= 0) {
      val beta = BetaFn(a)(b)
      val (f_val, f_err) = GaussHypergeometric(a, 1-b, a+1, x)
      val prefactor = (Math.pow(x,a)/a)
      val result_val = prefactor * f_val / beta
      result_val
    } else {
      /* Apply continued fraction directly. */
      // note the log beta function is used in the GSL, we may need to add a custom implementation of log beta.
      // for now am just using the log of the current beta implementation
      val ln_beta = Math.log(BetaFn(a)(b))
      // In the method gsl_sf_log_1plusx_e is used instead of below.
      val ln_1mx = Math.log(1.0+x)
      val ln_x = Math.log(x)

      val ln_pre_val = -ln_beta + a * ln_x + b * ln_1mx
      val prefactor = Math.exp(ln_pre_val)

      if (x < (a+1.0)/(a+b+2.0)) {
        val cf = beta_cont_frac(a, b, x)
        val result_val = prefactor*cf / a
        result_val
      } else {
        /* Apply continued fraction after hypergeometric transformation. */
        val cf = beta_cont_frac(b,a,1.0-x)
        val term = prefactor * cf / b

        term
      }
    }

  }


  def op(x:Double, alpha:Double, beta:Double):Double = {
  // alter implementation of incomplete beta function refer to numerical recipes.
    incompleteBeta (x, alpha, beta)
  }

}
object IncompleteBetaFn {
  def apply(x:Double, alpha:Double, beta:Double) = new IncompleteBetaFn().op(x, alpha, beta)
}
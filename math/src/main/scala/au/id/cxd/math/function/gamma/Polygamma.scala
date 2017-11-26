package au.id.cxd.math.function.gamma

import au.id.cxd.math.function.exponential.ExpMult
import au.id.cxd.math.function.zeta.HurwitzZeta

/**
  * Compute the nth derivative for Psi or the Digamma function.
  *
  * Copied from the GSL library function gsl_sf_psi_n_e in psi.c line 768.
  * the dependency between functions here in the GSL is a bit messy.
  * the function calls log factorial gsl_sf_lnfact_e which is part of the log gamma family
  * and indirectly depends on log gamma itself for approximation.
  * at the same time, log gamma indirectly also depends on polygamma.
  * this is a circular reference. Hence the approximation function needs to be passed through at runtime.
  */
class Polygamma(approxFactorialFn: Double => (Double, Double)) {

  protected def psiX(n:Double, x:Double) = {
    val (hz, err) = HurwitzZeta(n + 1.0, x)
    // the dependency between functions here in the GSL is a bit messy.
    // the function calls log factorial gsl_sf_lnfact_e which is part of the log gamma family
    // and indirectly depends on log gamma itself for approximation.
    // at the same time, log gamma indirectly also depends on polygamma.
    // this is a circular reference.
    val (yf, err1) = LogFact(approxFactorialFn, n)
    val (expY, err2) = ExpMult(yf, err1, hz, err)
    if (n % 2.0 == 0.0) (-expY, err2)
    else (expY, err2)
  }

  def op(n: Double, x: Double):(Double,Double) = {
    if (n < 0 || x <= 0.0) {
      throw new IllegalArgumentException(s"n = $n must be geq 0 x =$x must be geq 0")
    }
    if (n == 0) Digamma(x)
    else if (n == 1) {
      Trigamma(approxFactorialFn, x)
    } else psiX(n,x)

  }

}

object Polygamma {
  def apply(approxFactorialFn: Double => (Double, Double), n: Double, x: Double):(Double,Double) =
    new Polygamma(approxFactorialFn).op(n, x)

  def build(approxFactorialFn: Double => (Double, Double)):Polygamma = new Polygamma(approxFactorialFn)
}

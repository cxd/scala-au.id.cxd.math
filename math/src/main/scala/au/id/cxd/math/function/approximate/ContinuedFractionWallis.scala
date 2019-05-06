package au.id.cxd.math.function.approximate

import au.id.cxd.math.function.Constants

import scala.annotation.tailrec

/**
  * ##import MathJax
  *
  * Use the wallis algorithm to evaluate a continued fraction
  *
  * $$
  * f_n = \frac{A_n}{B_n}
  * $$
  *
  * where
  * $A_{-1} = 1$, $B_{-1} = 0$
  * $A_0 = b_0$, $B_0 = 1$
  * $A_j = b_jA_j{-1} + a_jA_{j-2}$
  * $B_j = b_jB_{j-1} + a_jB_{j-2}$
  *
  * This approach appears in the numerical recipes section 5.2
  * and is used in the evaluation of exponents in the GSL in
  * exp.c exprel_n_CF line 35
  */
class ContinuedFractionWallis(afn: (Double, Double) => Double, bfn: (Double, Double) => Double, maxN: Double) {


  @tailrec final def continue(n1: Double, x: Double, fn1: Double, prevAn: Double, prevBn: Double, anm1: Double, bnm1: Double): Double = {
    if (n1 >= maxN) {
      fn1
    } else {
      val n = n1+1.0
      val an = afn(n, x)
      val bn = bfn(n, x)
      val a1 = prevAn
      val b1 = prevBn
      val a2 = anm1
      val b2 = bnm1
      val An = bn * a1 + an * a2
      val Bn = bn * b1 + an * b2
      val (an1, bn1, am1, bm1, am2, bm2) = if (Math.abs(An) > Constants.SQRT_DBL_MAX || Math.abs(Bn) > Constants.SQRT_DBL_MAX) {
        (An / Constants.SQRT_DBL_MAX,
          Bn / Constants.SQRT_DBL_MAX,
          a1 / Constants.SQRT_DBL_MAX,
          b1 / Constants.SQRT_DBL_MAX,
          a2 / Constants.SQRT_DBL_MAX,
          b2 / Constants.SQRT_DBL_MAX)
      } else {
        (An, Bn, a1, b1, a2, b2)
      }
      val old_fn = fn1
      val fn = an1 / bn1
      val del = old_fn / fn
      if (Math.abs(del - 1.0) < 2.0 * Constants.DBL_EPSILON) fn
      else continue(n, x, fn, an1, bn1, am1, bm1)
    }
  }

  def op(x:Double) = {
    // initialise first 2 steps
    val anm1 = 0.0
    val anm2 = 1.0
    val bnm1 = 1.0
    val bnm2 = 0.0
    val a1 = afn(0, x)
    val b1 = bfn(0,x)
    val an = b1*anm1 + a1*anm2
    val bn = b1*bnm1 + a1*bnm2
    val a2 = afn(1.0,x)
    val b2 = bfn(1.0,x)
    val an2 = b2*an + a2*anm1
    val bn2 = b2*bn + a2*bnm1
    val fn = an2/bn2
    // the recursion starts with n = 2
    continue(2.0, x, fn, an2, bn2, an, bn)
  }
}
object ContinuedFractionWallis {
  def apply(afn: (Double, Double) => Double, bfn: (Double, Double) => Double, maxN: Double) = {
    new ContinuedFractionWallis(afn, bfn, maxN)
  }
  def apply(x:Double, afn: (Double, Double) => Double, bfn: (Double, Double) => Double, maxN: Double) = {
    new ContinuedFractionWallis(afn, bfn, maxN).op(x)
  }

}
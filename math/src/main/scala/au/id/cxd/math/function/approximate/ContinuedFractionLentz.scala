package au.id.cxd.math.function.approximate

import au.id.cxd.math.function.Constants

import scala.annotation.tailrec


/** Use the modified lentz algorithm to evaluate a continued fraction.
  *
  * @param afn
  * the function (j:Double, x:Double, a_{j-1}:Double) => Double
  * to calculate the numerator a_j in the series.
  * @param bfn
  * the function (j:Double, x:Double, b_{j-1}:Double) => Double
  * to calculate the denominator b_j in the series
  * @param maxN
  * - the maximum allowed iterations
  */
class ContinuedFractionLentz(afn: (Double, Double, Double, Double) => Double, bfn: (Double, Double, Double, Double) => Double, maxN: Double) {

  private val small = Constants.DBL_EPSILON*Constants.DBL_EPSILON*Constants.DBL_EPSILON
  /**
    * tail recursive evaluation of the continued fraction around the point x
    * using the modified Lentz algorithm identified p 208 or numerical recipes.
    * @param n
    * @param x
    * @param f0
    * @param a0
    * @param b0
    * @param c0
    * @param d0
    * @return
    */
  @tailrec final def continue(n: Double, x: Double, f0: Double, a0: Double, b0: Double, c0: Double, d0: Double):Double = {
    if (n >= maxN) f0
    else {
      val a = afn(n, x, a0, b0)
      val b = bfn(n, x, a0, b0)
      val d = b + a * d0
      val dj =
        if (Math.abs(d) < small) small
        else d
      val c = b + a / c0
      val cj =
        if (Math.abs(c) < small) small
        else c
      val d1 = 1.0 / dj
      val deltaj = cj * d1
      val fj = f0 * deltaj
      if (Math.abs(deltaj - 1.0) < Constants.DBL_EPSILON) fj
      else continue(n+1.0, x, fj, a, b, cj, d1)
    }
  }

  def op(x:Double) = {
    // initial values
    val a0 = afn(0,x,0,0)
    val b0 = bfn(0,x,0,0)
    val f0 = b0
    val c0 = f0
    val d0 = 0.0
    continue(2.0, x, f0, a0, b0, c0, d0)
  }

  def op(x:Double, c0:Double, d0:Double, f0:Double) = {
    // initial values
    val a0 = afn(0,x,0,0)
    val b0 = afn(0,x,0,0)
    continue(2.0, x, f0, a0, b0, c0, d0)
  }


}
object ContinuedFractionLentz {
  def apply(afn: (Double, Double, Double,Double) => Double, bfn: (Double, Double, Double,Double) => Double, maxN: Double) = {
    new ContinuedFractionLentz(afn, bfn, maxN)
  }
  def apply(x:Double, afn: (Double, Double, Double,Double) => Double, bfn: (Double, Double, Double,Double) => Double, maxN: Double) = {
    new ContinuedFractionLentz(afn, bfn, maxN).op(x)
  }

  def apply(x:Double, c1:Double, d1:Double, f1:Double, afn: (Double, Double, Double,Double) => Double, bfn: (Double, Double, Double,Double) => Double, maxN: Double) = {
    new ContinuedFractionLentz(afn, bfn, maxN).op(x, c1, d1, f1)
  }
}
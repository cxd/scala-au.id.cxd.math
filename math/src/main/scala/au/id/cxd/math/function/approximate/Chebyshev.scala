package au.id.cxd.math.function.approximate

import au.id.cxd.math.function.Constants

/** ##import MathJax
  *
  * approximate evaluation of a function in the interval a to b using Chebyshev approximation.
  *
  * This is based on the GSL eval.c gsl_cheb_eval_err
  * as well as the Chebyshev Approximation section in Numerical recipes pp 233 - 238.
  *
  * Approximate the function $f(x)$ in the interval [a,b].
  *
  * The function $f(x)$ is approximated using a change of variable
  *
  * $$
  * y \sim \frac{x - 1/2 (b-a)}{1/2(b-a)}
  * $$
  *
  * And the original function is approximated as
  *
  * $$
  * f(x) \approx \left[\sum_{k=0}&#94;n c_k T_k(y)\right] - c_0/2
  * $$
  *
  *
  *
  */
class Chebyshev(val coeffs: List[Double], val a: Double, b: Double) {

  /**
    * Evaluate the approximation
    *
    * $$
    * \sum_{k=0}&#94;n c_k T_k(y) - c_0/2
    * $$
    * at the point
    * $$
    * y = [x - (b+a)/2]/[(b-a)/2]
    * $$
    *
    * @param x
    * @return
    */

  def op(x: Double) = {
    val y = (2.0 * x - a - b) / (b - a)
    val y2 = 2.0 * y
    val d = coeffs.tail.reverse.foldLeft(0.0, 0.0) {
      (accum, c) => {
        val temp = accum._1
        val d1 = y2 * accum._1 - accum._2 + c
        val d2 = temp
        (d1, d2)
      }
    }
    val result = y * d._1 - d._2 + 0.5 * coeffs.head
    val absc = coeffs.map(Math.abs).reduce(_ + _)
    val last = coeffs.reverse.head
    val err = Math.abs(last) + absc * Constants.DBL_EPSILON
    (result, err)
  }

}

object Chebyshev {
  /**
    * this operation is applied where the coefficients are known
    *
    * @param coeffs
    * @param a
    * @param b
    */
  def apply(coeffs: List[Double], a: Double, b: Double) = new Chebyshev(coeffs, a, b)

  /**
    * given a function it is possible to generate a set of coefficients using the
    * transformation above.
    *
    * this constructor is based on the example from numerical recipes page 236.
    *
    * @param fn
    * @param a
    * @param b
    * @param order
    */
  def apply(fn: Double => Double, a: Double, b: Double, order: Int) = {

    val bma = 0.5 * (b - a)
    val bpa = 0.5 * (b + a)

    val fvalues = for (k <- 0 to order) yield {
      val y = Math.cos(Math.PI * (k + 0.5) / order)
      val f = fn(y * bma + bpa)
      f
    }

    val coeffs = for (j <- 0 to order) yield {
      val pairs = fvalues.zip(for (k <- 0 to order) yield k)
      pairs.map { pair =>
        pair._1 * Math.cos(Math.PI * j * (pair._2 + 0.5) / order)
      }.reduce(_+_)
    }

    new Chebyshev(coeffs.toList, a, b)
  }
}
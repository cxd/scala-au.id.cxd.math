package au.id.cxd.math.function

import au.id.cxd.math.count.Factorial

/**
 * Beta function implemented as:
 * the beta function (from the gamma function)
 * $$
 *   B(a, b) = \frac{ (a - 1)!(b - 1)! }{ (a + b - 1)! }
 * $$
 * Created by cd on 5/11/14.
 */
class BetaFn {

  def op(a:Double, b:Double) = Factorial(Math.round(a) - 1.0) * Factorial(Math.round(b) - 1.0) / Factorial((Math.round(a) + Math.round(b)) - 1.0)

}
object BetaFn {
  def apply(a:Double)(b:Double) = new BetaFn().op(a, b)
}

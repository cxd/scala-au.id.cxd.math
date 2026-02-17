package au.id.cxd.math.function.beta

import au.id.cxd.math.function.gamma.GammaFn

/**
  * ##import MathJax
  *
  * Beta function implemented as:
  * the beta function (from the gamma function)
  * $$
  * B(a, b) = \frac{ (a - 1)!(b - 1)! }{ (a + b - 1)! }
  * $$
  *
  * alternately
  *
  * $$
  * B(a,b) = \Gamma(a)\Gamma(b) / \Gamma(a + b)
  * $$
  *
  * https://en.wikipedia.org/wiki/Beta_function#Properties
  *
  * Created by cd on 5/11/14.
  */
class BetaFn {

    def op(a: Double, b: Double) = {
      // For large values, use log beta to avoid overflow
      if (a > 100 || b > 100 || (a + b) > 100) {
        Math.exp(LogBetaFn(a, b))
      } else {
        (GammaFn(a) * GammaFn(b)) / GammaFn(a + b)
      }
    }

}

object BetaFn {
  def apply(a: Double)(b: Double) = new BetaFn().op(a, b)
}

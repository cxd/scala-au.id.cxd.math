package au.id.cxd.math.function

import au.id.cxd.math.count.Factorial

/**
 * The gamma function $\Rho(a)$ is equivalent to $(a - 1)!$
 * Created by cd on 5/11/14.
 */
class GammaFn {

  def op(a:Double) = Factorial(Math.round(a) - 1)

}
object GammaFn {
  def apply(a:Double) = new GammaFn().op(a)
}

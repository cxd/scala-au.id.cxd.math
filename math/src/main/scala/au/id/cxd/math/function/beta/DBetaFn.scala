package au.id.cxd.math.function.beta

import au.id.cxd.math.function.gamma.Digamma

/**
  * ##import MathJax
  *
  * a derivative of the beta function
  * leveraging the digamma function
  *
  * $$
  * \frac{\partial}{\partial x} B(x,y) = B(x,y)(\psi(x) - \psi(x+y))
  * $$
  *
  * https://en.wikipedia.org/wiki/Beta_function
  *
  */
class DBetaFn {

  def op(x:Double,y:Double) = {
    BetaFn(x)(y) * (Digamma(x)._1 - Digamma(x+y)._1)
  }
}
object DBetaFn {
  def apply(x:Double,y:Double) = new DBetaFn().op(x,y)
}

package au.id.cxd.math.function

import au.id.cxd.math.probability.continuous.Beta

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
  * The incomplete beta function can be written as above, the ratio of the beta distribution over the complete gamma function
  */
class IncompleteBetaFn {

  def op(x:Double, alpha:Double, beta:Double) = {
    val f = BetaFn(alpha)(beta)
    val dist = Beta(alpha, beta)
    val p = dist.pdf(x)
    p / f
  }

}
object IncompleteBetaFn {
  def apply(x:Double, alpha:Double, beta:Double) = new IncompleteBetaFn().op(x, alpha, beta)
}
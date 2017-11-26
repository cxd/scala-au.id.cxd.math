package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.beta.{BetaFn, IncompleteBetaFn}

/**
  * ##import MathJax
  *
  * The beta distribution
  *
  * Implements the pdf.
  * $$
  * f(y) = \frac{ y&#94;{\alpha - 1} (1-y)&#94;{\beta-1} } { Beta(\alpha, \beta) }
  * $$
  *
  * Where the $Beta$ is the beta function [[BetaFn]]
  *
  * Properties of the distribution:
  * Mean $\mu$:
  * $$
  * \frac{\alpha}{\alpha + \beta}
  * $$
  *
  * Variance $\sigma&#94;2$
  *
  * $$
  * \sigma&#94;2 = \frac{ \alpha\beta } { {(\alpha + \beta)&#94;2} (\alpha + \beta + 1)}
  * $$
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
  *
  * https://en.wikipedia.org/wiki/Beta_function#Properties
  *
  * Created by cd on 5/11/14.
  */
class Beta(val alpha: Double, val beta: Double) extends ContinuousDistribution {

  val betaVal = BetaFn(alpha)(beta)

  /**
    * the mean of the distribution
    *
    * @return
    */
  def mean(): Double = alpha / (alpha + beta)

  def variance(): Double = alpha * beta / (Math.pow(alpha + beta, 2.0) * (alpha + beta + 1))

  def pdf(y: Double): Double = Math.pow(y, alpha - 1) * Math.pow(1 - y, beta - 1) / betaVal

  def cdf(y:Double) :Double = {
    IncompleteBetaFn(y, alpha, beta)
  }

}

object Beta {
  def apply(alpha: Double, beta: Double) = new Beta(alpha, beta)
}

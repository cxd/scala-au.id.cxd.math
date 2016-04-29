package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.{NumericIntegral, BetaFn}

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
  * Where the $Beta$ is the beta function [[au.id.cxd.math.function.BetaFn]]
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
}

object Beta {
  def apply(alpha: Double, beta: Double) = new Beta(alpha, beta)
}

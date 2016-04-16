package au.id.cxd.math.probability.continuous

/**
  * ##import MathJax
  *
  * The exponential distribution is a special case of the Gamma distribution
  * where alpha is set to 1
  *
  * $$
  * f(y) = \frac{ e^-{y/\beta} } {\beta}
  * $$
  * $$
  * 0 \le y \le \infnty
  * $$
  *
  * Created by cd on 6/11/14.
  */
class Exponential(val beta: Double) extends Gamma(1.0, beta) {

}

object Exponential {
  def apply(beta: Double) = new Exponential(beta)
}


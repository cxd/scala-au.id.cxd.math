package au.id.cxd.math.probability.bayes

import au.id.cxd.math.probability.Distribution

/**
  * ##import MathJax
  *
  * A simplistic approach to the bayes rule allowing the
  * distributions to be combined in a manner of enumerating the
  * possible values for the conditional variable
  *
  * $$
  * P(B_i|A) = \frac{ P(A and B) } {P(A) }
  * $$
  *
  * For discrete variables this is the likelihood multiplied by the prior divided by the marginalised P(A)
  * $$
  * P(A) = \frac{ P(A|B_i) \times P(B_i) } { \sum_{j=1}^n P(A|B_j)\times P(B_j) }
  * $$
  *
  * For continuous values it is the marginalised value for P(A) is the integral \int_{-\infty}^\infty P(A|B)P(B)
  *
  *
  *
  * Created by cd on 26/03/2016.
  *
  * @param likelihood this is the discrete distribution for $P(A|B_i) $
  * @param prior      this is the discrete distribution for $P(B)$
  *
  *
  *
  */
class DiscreteBayes(val likelihood: Distribution, val prior: Distribution) {

  /**
    * calculate the posterior likelihood for a and b gibven the universe rangeB
    *
    * This is a simplification of the calculation for the distributions of A|B and B
    *
    * The likelihood of A given $B_j$ is given in the rangeB parameter $P(A|B_j)$ .
    *
    * The rangeB parameter has the $(B_j, P(A|B_j))$ tuples.
    *
    * @param a
    * @param b
    * @param rangeB
    * @return
    */
  def posterior(a: Double, b: Double, rangeB: Seq[(Double, Distribution)]) = {
    val numerator = likelihood.pdf(a) * prior.pdf(b)
    val range = rangeB map { pair => {
      val (b1, likelihood1) = pair
      likelihood1.pdf(a) * prior.pdf(b1)
    }
    }
    val denominator = range.sum
    denominator match {
      case 0.0 => 0.0
      case _ => numerator / denominator
    }
  }

}

object DiscreteBayes {

  def apply(likelihood: Distribution, prior: Distribution) = new DiscreteBayes(likelihood, prior)
}

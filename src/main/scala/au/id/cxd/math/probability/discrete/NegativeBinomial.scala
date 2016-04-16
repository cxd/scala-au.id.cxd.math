package au.id.cxd.math.probability.discrete

import scala.math._
import au.id.cxd.math.count.Choose

/**
  * ##import MathJax
  *
  * Created by cd on 7/09/2014.
  *
  * The Negative Binomial Distribution (class name NegativeBinomial) provides the probability of the $nth$ success or potentially $nth$ failure of a bernoulli trial. The parameters are $r$ representing the $r -1$ initial trials that where the successful and $y$ the total number of trials before the next success $r$ occurs. The distribution is calculated as follows:
  * $$
  * P(y;r) = {y - 1 \choose r - 1}p^rq^{y-r}
  * $$
  * where $y = r, r + 1, ...$\\\\
  * The simple properties of the distribution are:\\\\
  * Mean:$\mu = \frac{r}{p}$\\
  * Variance:$\sigma^2 = \frac{r(1-p)}{p^2}$\\
  *
  * r - the first success
  * p the probability
  * y - the number of trials when the rth success occurs
  *
  * y >= r
  *
  */
class NegativeBinomial(r: Double, p: Double) extends DiscreteDistribution {
  /**
    * distribution
    *
    * (y-1 C r-1) p^r q^(y-r)
    *
    * @param y
    * @return
    */
  def pdf(y: Double) = Choose(y - 1.0)(r - 1.0) * pow(p, r) * pow(1.0 - p, y - r)

  def mean() = r / p

  def variance() = r * (1.0 - p) / pow(p, 2.0)
}

object NegativeBinomial {
  def apply(r: Double)(p: Double) = new NegativeBinomial(r, p)
}

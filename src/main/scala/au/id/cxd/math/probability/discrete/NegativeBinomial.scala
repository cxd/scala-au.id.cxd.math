package au.id.cxd.math.probability.discrete

import scala.math._
import au.id.cxd.math.count.Choose

/**
 * Created by cd on 7/09/2014.
 *
 * r - the first success
 * p the probability
 * y - the number of trials when the rth success occurs
 *
 * y >= r
 *
 */
class NegativeBinomial(r:Double, p:Double) extends DiscreteDistribution {
  /**
   * distribution
   *
   * (y-1 C r-1) p^r q^(y-r)
   * @param y
   * @return
   */
  def pdf(y:Double) = Choose(y-1.0)(r-1.0) * pow(p, r) * pow(1.0-p, y-r)

  def mean() = r / p

  def stddev() = r * (1.0 - p) / pow(p, 2.0)
}

object NegativeBinomial {
  def apply(r:Double)(p:Double) = new NegativeBinomial(r,p)
}

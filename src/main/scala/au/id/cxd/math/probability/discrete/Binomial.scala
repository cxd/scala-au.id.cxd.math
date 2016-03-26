package au.id.cxd.math.probability.discrete

import scala.math._
import au.id.cxd.math.count.Choose

/**
 * Created by cd on 6/09/2014.
 *
 * Binomial distribution
 *
 * n - totalnumber of trials
 * p - probability of success
 *
 * y - number of trials
 * The binomial module has the parameters $p$ for the prior proportion of successes and $n$ for the total number of trials and calculates the probability of $y$ successes
 * $$
 * P(y; n; p) = \sum_{i=1}^n {n \choose y_i} p^y_i (1-p)^{n-y_i}
 * $$
 *
 */
class Binomial(n:Double, p:Double) extends DiscreteDistribution {

  /**
   * probability function
   * @param y
   * @return
   */
  def pdf(y:Double) = {
    if (y <= n) {
      Choose(n)(y) * pow(p,y) * pow((1-p),(n-y))
    } else 0.0
  }


  def mean():Double = n*p

  def variance():Double = n*p*(1-p)

}

object Binomial {

  def apply(n:Double)(p:Double) =new Binomial(n,p)
}

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

  def stddev():Double = n*p*(1-p)

}

object Binomial {

  def apply(n:Double)(p:Double) =new Binomial(n,p)
}

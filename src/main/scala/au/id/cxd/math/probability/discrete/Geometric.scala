package au.id.cxd.math.probability.discrete

import scala.math._
/**
 * Created by cd on 6/09/2014.
 *
 * Geometric distribution with 1 parameter p = probability
 */
class Geometric(p:Double) extends DiscreteDistribution {

  /**
   * probability function
   * @param y
   * @return
   */
  def pdf(y:Double) = pow((1-p),y)*p

  def mean():Double = 1.0/p

  def stddev():Double = (1.0-p)/pow(p,2.0)
}

object Geometric {
  def apply(p:Double) = new Geometric(p)
}
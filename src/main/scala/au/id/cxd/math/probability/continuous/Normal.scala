package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.NumericIntegral

import scala.math._

/**
 * Created by cd on 9/09/2014.
 *
 * the normal distribution
 *
 */
class Normal(mu:Double, varianceVal:Double) extends ContinuousDistribution {

  def pdf(y:Double):Double = {
    (1/sqrt(variance*2.0*Pi) ) * exp(-1.0 * pow(y - mu, 2.0) / (2.0*variance) )
  }

  def mean():Double = mu

  def variance():Double = varianceVal

}

object Normal {
  def apply(mu:Double)(variance:Double) = new Normal(mu, variance)
}

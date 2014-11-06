package au.id.cxd.math.probability.continuous

import scala.math._

/**
 * Created by cd on 9/09/2014.
 *
 * the normal distribution
 *
 */
class Normal(mu:Double, variance:Double) extends ContinuousDistribution {

  def pdf(y:Double):Double = {
    (1/sqrt(variance*2.0*Pi) ) * exp(-1.0 * pow(y - mu, 2.0) / (2.0*variance) )
  }

  def mean():Double = mu

  def stddev():Double = sqrt(variance)

  /**
   * since the normal dist cannot be integrated, the integral needs to be approximated
   * as the sum of the pdf for very small increments between start and end
   * @param start
   * @param end
   */
  def integral(start:Double, end:Double):Double = approxIntegral(start, end)(pdf)

}

object Normal {
  def apply(mu:Double)(variance:Double) = new Normal(mu, variance)
}

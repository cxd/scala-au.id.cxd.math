package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.NumericIntegral
import au.id.cxd.math.probability.Distribution
import breeze.linalg.{max, min}

/**
 * Created by cd on 9/09/2014.
 */
trait ContinuousDistribution extends Distribution {

  def mean():Double

  def stddev():Double

  /**
    * integral from start to end for the pdf
    * @param start
    * @param end
    * @return
    */
  def integral(start: Double, end: Double): Double = NumericIntegral(start, end, pdf(_)).integrate()


  def cdf(y:Seq[Double]):Double = {
    val start = min(y.head, y.last)
    val end = max(y.head, y.last)
    integral(start, end)
  }
}

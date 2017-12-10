package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.approximate.NumericIntegral
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
    *
    * This is equivalent to cdf(end) - cdf(start)
    *
    * @param start
    * @param end
    * @return
    */
  def integral(start: Double, end: Double): Double = {
    /*val a = cdf(start)
    val b = cdf(end)
    b - a
    */
    NumericIntegral(start, end, pdf(_)).integrate()
  }

  /**
    * cumulative density function
    * @param y
    * @return
    */
  def cdf(y: Seq[Double]): Double = integral(y.head, y.tail.head)

  /**
    * calculate the cdf $P( < y)$.
    * @param y
    * @return
    */
  def cdf(y:Double):Double


  /**
    * estimate the quartile y for the distribution given the
    * percentage of the distribution
    * @param p
    * @return
    */
  def invcdf(p:Double):Double = throw new NotImplementedError("method is not implemented for this distribution")



}

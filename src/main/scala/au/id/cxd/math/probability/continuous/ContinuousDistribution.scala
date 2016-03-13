package au.id.cxd.math.probability.continuous

import au.id.cxd.math.probability.Distribution
import breeze.linalg.{max, min}

/**
 * Created by cd on 9/09/2014.
 */
trait ContinuousDistribution extends Distribution {

  def mean():Double

  def stddev():Double

  /**
   * approximate the integral using the summation using the midpoint rule
   * a = x+deltax
   * b = x+2deltax
   * w = (a+b)/2
   * p = [pdf(a)+pdf(b)]*w
   * @param start
   * @param end
   * @return
   */
  def approxIntegral(start:Double, end:Double)(pdf:(Double) => Double):Double = {
    (min(start, end) to max(start, end-0.01) by 0.01)
      .zip(start+0.01 to end by 0.01)
      .map {
        (pair:(Double, Double)) => {
          val a = pdf(pair._1)
          val b = pdf(pair._2)
          val w = (pair._2-pair._1)/2.0
          w*(a+b)
    }
      }.sum
  }


  def integral(start:Double, end:Double):Double;


  def cdf(y:Seq[Double]):Double = {
    val start = min(y.head, y.last)
    val end = max(y.head, y.last)
    integral(start, end)
  }
}

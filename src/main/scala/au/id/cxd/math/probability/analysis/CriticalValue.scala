package au.id.cxd.math.probability.analysis

import au.id.cxd.math.probability.Distribution
import au.id.cxd.math.probability.continuous.ContinuousDistribution

import scala.annotation.tailrec
import scalaz.Memo

class Region {}

case class UpperTail() extends Region {}

case class LowerTail() extends Region {}



/**
  * An abstract class with a dependency on a distributon
  * it is used to tabulate the distribution to find a value that corresponds
  * to the critical region.
  *
  * critical value supports both continuous and discrete distributions
  *
  * The critical value searches for the input value that corresponds with the alpha value.
  *
  * Given a distribution find Y where P(X <= Y) = alpha
  *
  * Alpha is commonly defined as 0.05 for tests.
  *
  * The value that results is the critical value, and is used as a rejection region
  * in performing hypothesis testing.
  *
  * This is similar to the q* class of functions in R for example qnorm(0.05, 0, 1) or qf(0.05, 1, 10)
  *
  * This is the inversion of the probability mass function P(X <= Y; params) = alpha
  *
  * we need to find f(alpha, X, params) = Y
  *
  * It uses the CDF since the CDF computes P(X <= Y) = alpha
  *
  * For the upper tail the CDF is inverted so that P(X >= Y) = alpha
  * P(X >= Y) = 1 - CDF(X)
  *
 * Created by cd on 11/11/14.
 */
class CriticalValue(val cdf: Seq[Double] => Double, val region: Region, val range: Seq[Double]) {

  /**
   *  searching the upper end involves computing the CDF for the
   *  upper tail of the curve. The range parameter has been reversed prior to calling
   *  this function so that the sequence is in descending order rather than ascending.
   * @param max
   * @param prob
   * @param range
   * @return
   */
  @tailrec
  final def searchUpper (max:Double, prob:Double)(range:Seq[Double]):Double = {
    val r = cdf(Seq(range.head, max))
    if (r >= prob) range.head
    else searchUpper(max, prob)(range.tail)
  }

  /**
   * the lower tail of the curve
   * @param min
   * @param prob
   * @param range
   * @return
   */
  @tailrec
  final def searchLower (min:Double, prob:Double)(range:Seq[Double]):Double = {
    val r = cdf(Seq(min, range.head))
    if (r >= prob) range.head
    else searchLower(min, prob)(range.tail)
  }

  /**
   * return the critical value for the current cdf and probability
   * @return
   */
  def value: (Double => Double) = {
    def innerOp(region: Region)(prob: Double) = {
      val c = region match {
        case UpperTail() => {
          val reverse = range.reverse
          searchUpper(reverse.head, prob)(reverse.tail)
        }
        case LowerTail() => searchLower(range.head, prob)(range.tail)
      }
      c
    }
    // TODO: determine how to memoize the pair (region x double) since we want either upper or lower tail critical regions
    //Memo.mutableHashMapMemo {
    innerOp(region)
    //}
  }

}

object CriticalValue {
  def apply(cdf: Seq[Double] => Double, region: Region)(range: Seq[Double]) = new CriticalValue(cdf, region, range)
}

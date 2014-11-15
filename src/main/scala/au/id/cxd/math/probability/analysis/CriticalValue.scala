package au.id.cxd.math.probability.analysis

import scala.annotation.tailrec
import scalaz.Memo

class Region {}

case class UpperTail() extends Region {}

case class LowerTail() extends Region {}

/**
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

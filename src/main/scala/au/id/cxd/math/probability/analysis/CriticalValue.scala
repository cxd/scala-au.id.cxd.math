package au.id.cxd.math.probability.analysis

import scalaz.Memo

class Region {}

case class UpperTail() extends Region {}

case class LowerTail() extends Region {}

/**
 * Created by cd on 11/11/14.
 */
class CriticalValue(val cdf: Seq[Double] => Double, val region: Region, val range: Seq[Double]) {

  /**
   * return the critical value for the current cdf and probability
   * @return
   */
  def value: (Double => Double) = {
    def innerOp(region: Region)(prob: Double) = {
      // take the first value of the operation
      // TODO: work on perhaps a search procedure for the critical value that is not sequential perhaps split the search space successively in half
      range.map(i => (i, i + 0.01))
        .map(pair => {
        val c = region match {
          case UpperTail() => 1.0 - cdf(Seq(pair._1, pair._2))
          case LowerTail() => cdf(Seq(pair._1, pair._2))
        }
        (pair._2, c)
      })
        .filter(pair => pair._2 >= prob)
        .head
        ._2
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

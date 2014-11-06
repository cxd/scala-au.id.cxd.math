package au.id.cxd.math.probability.discrete

import au.id.cxd.math.probability.Distribution

/**
 * Created by cd on 6/09/2014.
 */
trait DiscreteDistribution extends Distribution {

  /**
   * cumulative distribution function
   * @param range
   * @return
   */
  def cdf(range:Seq[Double]):Double = {
    range.map (pdf).sum
  }

}

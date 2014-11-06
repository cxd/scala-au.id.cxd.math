package au.id.cxd.math.probability.discrete

import au.id.cxd.math.count.Choose

/**
 * Created by cd on 7/09/2014.
 *
 * Hyper geometric
 *
 * N - population size
 * n - sample size
 *
 * r - size of subset of items or events
 * y - selected number events
 *
 * r <= n <= N
 * y <= r <= n
 *
 */
class HyperGeometric(rSubsetSize:Double, sampleSize:Double, populationSize:Double) extends DiscreteDistribution {

  /**
   *
   * r = size of subset of items or events
   * y = selected number of items
   *
   * y <= r
   *
   * @param selectSize
   * @return
   */
  def pdf(selectSize:Double) =
    Choose(rSubsetSize)(selectSize) * Choose(populationSize - rSubsetSize)(sampleSize - selectSize) / Choose(populationSize)(sampleSize)

  def mean() = sampleSize*rSubsetSize / populationSize

  def stddev() = sampleSize * (rSubsetSize/populationSize) * ((populationSize - rSubsetSize)/populationSize) * ((populationSize - sampleSize)/(populationSize - 1.0))

}

object HyperGeometric {
  def apply(r:Double)(n:Double)(N:Double) = new HyperGeometric(r, n , N)
}

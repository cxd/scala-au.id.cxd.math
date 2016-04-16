package au.id.cxd.math.probability.discrete

import au.id.cxd.math.count.Choose

/**
  * ##import MathJax
  *
  * Created by cd on 7/09/2014.
  *
  * Hyper geometric
  *
  *
  * The Hypergeometric (class name HyperGeometric) distribution represents the probability of choosing $y$ number of events of the same kind
  * from a subset of $r$ like items within a population of all $N$ possible items (of different kinds) for the sample of size $n$ containing
  * the mixed items.
  *
  * The constraints are such that $r \le n \le N$ and $y \le r \le n$. The parameters are $y,r,n,N$.
  *
  * The probability distribution is defined as follows.
  * $$
  * P(y; r,n,N) = \frac{ {r \choose y } {{N - r} \choose {n - y} } } {  {N \choose n} }
  * $$
  *
  * The simple properties of the distribution are:\\\\
  * Mean: $\mu = \frac{nr}{N}$\\
  * Variance: $\sigma^2 = n \left( \frac{r}{N} \right) \left( \frac{N - r}{N} \right) \left( \frac{N - n}{N-1}\right)$
  *
  *
  * r <= n <= N
  * y <= r <= n
  *
  **/
class HyperGeometric(rSubsetSize: Double, sampleSize: Double, populationSize: Double) extends DiscreteDistribution {

  /**
    *
    * calculate the probability of selecting y (select Size) samples
    *
    * @param selectSize
    * @return
    */
  def pdf(selectSize: Double) =
    Choose(rSubsetSize)(selectSize) * Choose(populationSize - rSubsetSize)(sampleSize - selectSize) / Choose(populationSize)(sampleSize)

  def mean() = sampleSize * rSubsetSize / populationSize

  def variance() = sampleSize * (rSubsetSize / populationSize) * ((populationSize - rSubsetSize) / populationSize) * ((populationSize - sampleSize) / (populationSize - 1.0))

}

object HyperGeometric {
  def apply(r: Double)(n: Double)(N: Double) = new HyperGeometric(r, n, N)
}

package au.id.cxd.math.probability.continuous

/**
  * Gumbel distribution
  * https://en.wikipedia.org/wiki/Gumbel_distribution
  * @param location
  * @param scale parameter > 0
  */
class Gumbel(var location:Double = 0.0, var scale:Double = 1.0) extends ContinuousDistribution {
  // https://en.wikipedia.org/wiki/Euler%27s_constant
  val em_constant = 0.577
  val variance_scalar = 6

  override def mean(): Double = location + em_constant*scale

  /**
    * calculate the cdf $P( < y)$.
    *
    * @param y
    * @return
    */
override def cdf(y: Double): Double = {
  val e = Math.exp(-(y - location)/scale)
  val c = Math.exp(-e)
  c
}

  /**
    * probability density function
    *
    * @param y
    * @return
    */
override def pdf(y: Double): Double = {
  val z = (y - location)/scale
  val e = Math.exp(-z)
  val p = (1/scale) * Math.exp(-(z + e))
  p
}

  /**
    * variance for the distribution $\sigma&#94;2$
    *
    * @return
    */
  override def variance(): Double = (Math.pow(Math.PI, 2.0) / variance_scalar)*Math.pow(scale,2.0)
}

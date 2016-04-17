package au.id.cxd.math.probability

/**
  * ##import MathJax
  *
  * The trait for all distribution functions, each distribution has
  * the probability density function, the cumulative density function, mean and variance.
  *
  * Created by cd on 6/09/2014.
  */
trait Distribution {

  /**
    * probability density function
    * @param y
    * @return
    */
  def pdf(y: Double): Double

  /**
    * cumulative density function
    * @param y
    * @return
    */
  def cdf(y: Seq[Double]): Double

  /**
    * mean $\mu$ of the distribution
    * @return
    */
  def mean(): Double

  /**
    * variance for the distribution $\sigma^2$
    * @return
    */
  def variance(): Double

  /**
    * standard deviation $\sigma$
    * @return
    */
  def stddev(): Double = Math.sqrt(variance)


}

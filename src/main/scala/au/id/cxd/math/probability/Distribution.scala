package au.id.cxd.math.probability

/**
 * Created by cd on 6/09/2014.
 */
trait Distribution {

  def pdf(y:Double):Double

  def cdf(y:Seq[Double]):Double

  def mean():Double

  def variance():Double

  def stddev():Double = Math.sqrt(variance)



}

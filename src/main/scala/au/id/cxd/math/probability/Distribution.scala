package au.id.cxd.math.probability

/**
 * Created by cd on 6/09/2014.
 */
trait Distribution {

  def pdf(y:Double):Double;

  def cdf(y:Seq[Double]):Double;

  def mean():Double;

  def stddev():Double;


}

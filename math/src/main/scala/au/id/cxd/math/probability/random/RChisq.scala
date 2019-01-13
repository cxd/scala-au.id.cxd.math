package au.id.cxd.math.probability.random

class RChisq(val df:Double) extends RandomDeviate {

  val gamma = RGamma(0.5*df.toDouble, 0.5)

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = gamma.draw()
}
object RChisq {
  def apply(df:Double) = new RChisq(df)
}

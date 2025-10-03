package au.id.cxd.math.probability.random

import au.id.cxd.math.probability.continuous.ChiSquare

class RChisq(override val df:Double) extends ChiSquare(df)
  with  RandomDeviate {

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

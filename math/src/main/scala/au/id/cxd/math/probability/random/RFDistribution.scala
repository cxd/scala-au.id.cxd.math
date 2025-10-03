package au.id.cxd.math.probability.random

import au.id.cxd.math.probability.continuous.FDistribution

class RFDistribution(override val numeratorDf:Double, override val denominatorDf:Double)
  extends FDistribution(numeratorDf, denominatorDf)
    with RandomDeviate {

  val rbeta = RBeta(0.5*numeratorDf.toDouble, 0.5*denominatorDf.toDouble)

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    val x = rbeta.draw()
    denominatorDf*x / (numeratorDf*(1.0 - x))
  }
}
object RFDistribution {
  def apply(numeratorDf:Double, denominatorDf:Double) = new RFDistribution(numeratorDf, denominatorDf)
}
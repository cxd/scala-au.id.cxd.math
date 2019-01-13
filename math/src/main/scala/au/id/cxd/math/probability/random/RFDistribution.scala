package au.id.cxd.math.probability.random

class RFDistribution(val numeratorDf:Int, val denominatorDf:Int) extends RandomDeviate {

  val beta = RBeta(0.5*numeratorDf.toDouble, 0.5*denominatorDf.toDouble)

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    val x = beta.draw()
    denominatorDf*x / (numeratorDf*(1.0 - x))
  }
}
object RFDistribution {
  def apply(numeratorDf:Int, denominatorDf:Int) = new RFDistribution(numeratorDf, denominatorDf)
}
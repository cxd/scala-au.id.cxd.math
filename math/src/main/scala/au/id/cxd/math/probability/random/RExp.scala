package au.id.cxd.math.probability.random


/**
 * Filename: RExp
 * Created by cd on 3/10/2025
 *
 * */
class RExp(override val beta:Double) extends RGamma(1.0, beta) with RandomDeviate {

}

object RExp {
  def apply(beta:Double) = new RExp(beta)
}
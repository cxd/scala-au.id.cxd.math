package au.id.cxd.math.probability.random

import scala.annotation.tailrec

/**
  * raw a random deviate from the gamma function $\Gamma(\alpha)$
  * Base on the implementation from the GSL gamma.c line 65
  *
  * @param alpha
  */
class RGammaFn(val alpha: Double) extends RandomDeviate {



  val uniformPos = RUniformPos()

  def gammaSmall(): Double = {

    @tailrec
    def inner(i: Double, prod: Double): Double = {
      if (i >= alpha) {
        -Math.log(prod)
      } else {
        val prod1 = prod * uniformPos.draw()
        inner(i + 1.0, prod1)
      }
    }

    inner(0.0, 1.0)
  }

  /**
    * rejection method defined in GSL gamma.c line 91
    *
    * From GSL:
    * " Works only if a > 1, and is most efficient if a is large
    *
    * This algorithm, reported in Knuth, is attributed to Ahrens.  A
    * faster one, we are told, can be found in: J. H. Ahrens and
    *      U. Dieter, Computing 12 (1974) 223-246."
    *
    * @return
    */
  def gammaLarge(): Double = {
    val uniform = RUniform()

    @tailrec
    def computeX(sqa: Double): (Double, Double) = {
      val y1 = Math.tan(Math.PI * uniform.draw())
      val x1 = sqa * y1 + alpha - 1.0
      if (x1 > 0.0) (x1, y1)
      else computeX(sqa)
    }

    @tailrec
    def inner(sqa: Double): Double = {
      val (x, y) = computeX(sqa)
      val v = uniform.draw()
      if (v <= (1.0 + y * y) * Math.exp((alpha - 1.0) * Math.log(x / (alpha - 1.0)) - sqa * y)) x
      else inner(sqa)
    }

    val sqa = Math.sqrt(2.0*alpha - 1.0)
    inner(sqa)
  }

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double =
    if (alpha < 12.0) gammaSmall()
    else gammaLarge()
}
object RGammaFn {
  def apply(alpha:Double) = new RGammaFn(alpha)
}
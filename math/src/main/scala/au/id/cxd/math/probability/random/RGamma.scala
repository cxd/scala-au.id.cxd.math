package au.id.cxd.math.probability.random

/**
  * Generate a random gamma deviate.
  * Based on the procedure of Marsaglia and Tsang. 'A simple method for generating gamma variables' ACM Transactions on Mathematical Software Sept 2000.
  *
  * https://doi.org/10.1145/358407.358414
  *
  * A wide variety of example implementations are available such as the one outlined here: https://gist.github.com/fasiha/f289c1d4b9c244a42abee54d1d6206ed
  *
  * The method makes use of a rejection sampling method.
  *
  * The core procedure is:
  *
  * - generate normal x and uniform U
  * - until ln(U) < 0.5x^2 + d - dv + dln(v)
  * - return dv
  *
  * Where d = \alpha - 1/3 and v = (1 + x/\sqrt{9x^2}
  *
  *
  * The RGamma can be used as the building block for random variates of beta, chisq and the f-distribution and can be used
  * to generate samples from the student-t distribution.
  *
  *
  * I haven't yet implemented a test for gamma distribution goodness of fit.
  * visually inspecting the samples produced from above and
  * using rgamma(100, alpha, beta) in R produces similar density plots.
  *
  * There is a test proposed in https://doi.org/10.1016/j.spl.2014.10.001
  * which could be used to test for membership of gamma distribution.
  * It would be useful to implement for univariate tests of measurements from gamma.
  * The article also proposes methods of approximating alpha and beta from a gamma distribution
  * with unknown parameters.
  *
  * @param alpha
  * @param beta
  */
class RGamma(val alpha: Double, val beta: Double) extends RandomDeviate {

  val alpha1: Double =
    if (alpha < 1.0) alpha + 1.0
    else alpha

  val d: Double = alpha1 - 1.0 / 3.0

  val c: Double = 1.0 / Math.sqrt(9.0 * d)

  val norm = RNormal()

  val uni = RUniform()

  /**
    * rejection sampling for normal distribution.
    *
    * @return
    */
  def sampleNorm(): (Double, Double) = {
    val samples = Stream.from(1).map { _ =>
      val x = norm.draw()
      val v = 1.0 + c * x
      // v is v^3
      (x, v * v * v)
    }
    val (x, v) = samples
      .filter(pair => pair._2 > 0)
      .take(1)
      .head
    (x, v)
  }

  /**
    * use the rejection sampling method proposed by Marsaglia and Tsang
    * apply the squeezes used to approximate the logarithms
    *
    * @return
    */
  def sampleMarsagliaTsang(): Double = {

    val samples = Stream.from(1).map { _ =>
      val (x, v) = sampleNorm()
      val u = uni.draw()
      (x, v, u)
    }
    val dv = samples
      .filter(pair => {
        val (x, v, u) = pair
        val flag1 = v > 0.0
        // squeeze
        val flag2 = u < 1.0 - 0.0331 * x * x * x * x
        val flag3 = Math.log(u) < 0.5 * x * x + d * (1.0 - v + Math.log(v))
        flag1 && flag2 || flag1 && flag3
      }).map(pair => {
      val (x, v, u) = pair
      d * v
    }).take(1)
      .head
    dv
  }


  /**
    * random draw from Gamma(alpha,beta) distribution
    *
    * @return
    */
  override def draw(): Double = {
    val dv = sampleMarsagliaTsang()
    dv / beta
  }
}

object RGamma {
  def apply(alpha: Double, beta: Double) = new RGamma(alpha, beta)
}
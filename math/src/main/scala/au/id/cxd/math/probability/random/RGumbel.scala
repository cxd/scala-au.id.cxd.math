package au.id.cxd.math.probability.random

import scala.annotation.tailrec
import scala.util.Random

/**
  * Random Gumbel distributed data.
  * This uses the built in random function with a min max parameter.
 * Based on implementation numpy distributions.c random_gumbel
  * https://numpy.org/doc/2.1/reference/random/c-api.html
  * @param loc
  * @param scale
  */
class RGumbel(val loc:Double=0.0, val scale:Double=1.0, val seed:Long = 0L) extends RandomDeviate {
  val rand = if (seed != 0L) {
    val rand1 = Random
    rand1.setSeed(seed)
    rand1
  } else Random

  /**
    * sample from the range.
    * @return
    */
  final def sample():Double = {
    val u = 1.0 - rand.nextDouble()
    // u is distributed between 0 and 1
    loc - scale * Math.log(-Math.log(u))
  }

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = sample()

}
object RGumbel {
  def apply(loc:Double=0.0, scale:Double=1.0, seed:Long = 0L) = new RGumbel(loc, scale, seed)
}

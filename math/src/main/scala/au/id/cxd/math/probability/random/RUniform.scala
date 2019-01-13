package au.id.cxd.math.probability.random

import scala.util.Random

/**
  * Random draw from a uniform distribution.
  * This uses the built in random function with a min max parameter.
 *
  * @param min
  * @param max
  */
class RUniform(val min:Double=0.0, val max:Double=1.0, val seed:Long = 42L) extends RandomDeviate  {
  val rand = Random
  rand.setSeed(seed)

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    val next = rand.nextDouble()
    val delta = max - min
    min + delta * next
  }

}
object RUniform {

  def apply(min:Double=0.0, max:Double=1.0, seed:Long=42L) = new RUniform(min, max, seed)
}

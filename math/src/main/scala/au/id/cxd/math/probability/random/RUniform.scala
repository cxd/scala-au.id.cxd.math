package au.id.cxd.math.probability.random

import scala.util.Random

/**
  * Random draw from a uniform distribution.
  * This uses the built in random function with a min max parameter.
 *
  * @param min
  * @param max
  */
class RUniform(val min:Double, val max:Double, val seed:Long = 42L) extends RandomDeviate  {
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

  def apply(min:Double, max:Double, seed:Long=42L) = new RUniform(min, max, seed)
}

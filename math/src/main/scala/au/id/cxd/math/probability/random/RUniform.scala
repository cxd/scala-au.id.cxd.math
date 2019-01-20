package au.id.cxd.math.probability.random

import scala.annotation.tailrec
import scala.util.Random

/**
  * Random draw from a uniform distribution.
  * This uses the built in random function with a min max parameter.
 *
  * @param min
  * @param max
  */
class RUniform(val min:Double=0.0, val max:Double=1.0, val seed:Long = 0L, val allowMax:Boolean = false) extends RandomDeviate  {
  val rand = if (seed != 0L) {
    val rand1 = Random
    rand1.setSeed(seed)
    rand1
  } else Random

  /**
    * sample from the range.
    * @return
    */
  def sample():Double = {
    val next = rand.nextDouble()
    val delta = max - min
    min + delta * next
  }

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    @tailrec
    def inner():Double = {
      val x = sample()
      if (!allowMax && x != max) x
      else inner()
    }
    val x = inner()
    x
  }

}
object RUniform {

  def apply(min:Double=0.0, max:Double=1.0, seed:Long=0L, allowMax:Boolean=false) = new RUniform(min, max, seed, allowMax)
}

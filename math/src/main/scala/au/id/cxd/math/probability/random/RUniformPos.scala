package au.id.cxd.math.probability.random

import scala.annotation.tailrec

/**
  * random uniform sampling in distribution (0,1) excludes 0 and 1.
  * @param min
  * @param max
  * @param seed
  */
class RUniformPos()
  extends RUniform(min=0.0,max=1.0,seed=0L) {


  override def draw(): Double = {
    @tailrec
    def inner():Double = {
      val x = super.draw()
      if (x != 0.0 && x != 1.0) x
      else inner()
    }
    inner()
  }

}

object RUniformPos {

  def apply() = new RUniformPos()
}

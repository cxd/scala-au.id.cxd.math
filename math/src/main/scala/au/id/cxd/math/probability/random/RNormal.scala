package au.id.cxd.math.probability.random

class RNormal(val mu: Double = 0.0, val sigma: Double = 1.0, val seed: Long = 42L) extends RandomDeviate {
  val runif = RUniform(-1.0, 1.0)

  /**
    * offset by means and multiply by variance.
    * @param p
    * @return
    */
  def scale(p: Double):Double = mu + sigma * p

  /**
    * sample from a square region (-1,-1) to (1,1)
    * @return
    */
  def unitCircle(): (Double,Double,Double) = {
    val candidates = Stream.from(1).map { _ => {
      /* choose x,y in uniform square (-1,-1) to (+1,+1) */
      val x = runif.draw
      val y = runif.draw
      /* see if it is in the unit circle */
      val r2 = x * x + y * y
      if (r2 > 1.0 || r2 == 0) {
        (x, y, r2, false)
      } else (x, y, r2, true)
    }
    }
    val sample = candidates
      .filter(p => p._4 == true)
      .take(1)
      .head
    (sample._1,sample._2, sample._3)
  }

  /**
    * random draw from a normal distribution using the box-muller technique.
    *
    * @return
    */
  override def draw(): Double = {
    val (x,y,r2) = unitCircle()
    val r = Math.sqrt(-2.0 * Math.log(r2) / r2)
    val p = y * r
    scale(p)
  }
}

object RNormal {
  def apply(mu: Double = 0.0, sigma: Double = 1.0, seed: Long = 42L) = new RNormal(mu, sigma, seed)
}

package au.id.cxd.math.probability.random

/**
  * a random deviate.
  */
trait RandomDeviate {

  /**
    * random draw from a distribution
    * @return
    */
  def draw():Double

  /**
    * draw n times.
    *
    * @param n
    * @return
    */
  def draw(n: Int): Seq[Double] = for (i <- 1 to n) yield draw()

  /**
    * an unbounded sequence of random draws
    *
    * @return
    */
  def generate(): Seq[Double] = Stream.from(1) map (_ => draw())

}

package au.id.cxd.math.probability.random

import scala.annotation.tailrec

/**
  * An approximate random draw from the binomial distribution with parameters n and p
  *
  * This is based on the commentry given in the GSL implementation of the randist binomial
  * which will be implemented in RBinom.
  *
  * This method if given for fast approximations derived from the normal distribution with mean n*p and variance n*p*(1-p)
  *
  * The results are rounded to integers
  */
class RApproxBinom(val n:Double, val p:Double) extends RandomDeviate {

  val norm = RNormal(n*p, n*p*(1.0-p))

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    @tailrec
    def innerDraw():Double = {
      val d = Math.round(norm.draw()).toDouble
      if (d >= 0) d
      else innerDraw()
    }
    innerDraw()
  }
}
object RApproxBinom {
  def apply(n:Double, p:Double) = new RApproxBinom(n,p)
}

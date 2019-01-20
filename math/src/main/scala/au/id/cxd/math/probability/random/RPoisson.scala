package au.id.cxd.math.probability.random

import scala.annotation.tailrec

/**
  * Random deviate from the poisson distribution based on the GSL implementation.
  * File poisson.c Line 26. gsl_ran_gamma
  *
  * The poisson distribution has the form
  *
  * p(n) = (mu^n / n!) exp(-mu)
  *
  * for n = 0, 1, 2, ... . The method used here is the one from Knuth.
  *
  * The implementation of gsl_ran_poisson is somewhat more concise than that given in the Numerical Recipes.
  *
  * @param lambda
  */
class RPoisson(val lambda: Double) extends RandomDeviate {

  val uniform = RUniform()

  /**
    * seminumerical algorithms knuth p137 "Numerical Distributions".
    * @param lambda
    * @return
    */
  def calculateLargeK(lambda:Double) : Double = {
    @tailrec
    def calculateK(k:Double, mu:Double):Double = {
      val m = Math.round(mu * (7.0/8.0))
      val x = RGammaFn(alpha=m).draw()
      if (x >= mu) k + RBinom(n=m-1.0, p=mu/x).draw()
      else if (mu <= 10.0) calculateSmallK(mu, k)
      else calculateK(k + m, mu - x)
    }
    Math.round(calculateK(0.0, lambda))
  }

  def calculateSmallK(lambda:Double, k:Double):Double = {
    @tailrec
    def calculateK(k:Double, prod:Double, emu:Double):Double = {
      val prod1 = prod * uniform.draw()
      val k1 = k + 1.0
      if (prod1 <= emu) k1
      else calculateK(k1,prod1, emu)
    }

    val emu = Math.exp(-lambda)

    val k1 = calculateK(k, prod=1.0, emu)
    Math.round(k1) - 1.0
  }

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
    if (lambda > 10) calculateLargeK(lambda)
    else calculateSmallK(lambda, 0.0)
  }
}
object RPoisson {
  def apply(lambda:Double) = new RPoisson(lambda)
}

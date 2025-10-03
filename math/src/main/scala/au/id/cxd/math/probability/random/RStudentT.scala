package au.id.cxd.math.probability.random

import au.id.cxd.math.probability.continuous.StudentT
/**
  * Draw random deviate from student t distribution.
  * Given in numerical methods as
  *
  * x \sim N(0,1)
  * y \sim Gamma(\nu/2, 1/2)
  *
  * x\sqrt{\nu/y} \sim StudentT(\nu, 0, 1)
  *
  * @param df
  * @param mu
  * @param sigma
  */
class RStudentT(override val df: Double = 1.0, override val mu: Double, override val sigma: Double)
  extends StudentT(df, mu, sigma) with RandomDeviate {

  val gamma = RGamma(alpha = df / 2.0, beta = 1.0 / 2.0)
  val norm = RNormal()

  def draw(): Double = {
    val y = gamma.draw()
    val x = norm.draw()
    val v = x * Math.sqrt(df / y)
    mu + sigma * v
  }
}
object RStudentT {
  def apply(df:Double = 1.0, mu:Double=0.0, sigma:Double = 1.0) = new RStudentT(df, mu, sigma)
}
package au.id.cxd.math.function.gamma

import au.id.cxd.math.function.Constants

/**
  * #import MathJax
  *
  * Implementation of the error function
  *
  * $$
  * erf(x) = \frac{2}{\pi} \int_0&#94;x e&#94;{-t&#94;2}dt
  * $$
  *
  * based on the GSL implementation file erfc.c line 357 gsl_sf_erf_e
  *
  * http://mathworld.wolfram.com/Erfc.html
  */
class Erf {

  /**
    * approximation of error function via series
    * as per gsl erfc.c line 123
    * @param x
    */
  def erfseries(x:Double) = {
    val (coef, e) = {
      for (k <- 1 to 30)
        yield (-x*x/k, k)
    }.foldLeft ((x, x)) { (accum, c) => {
      val c1 = accum._1 * c._1
      val e1 = accum._1 + c._1/(2.0*accum._2+1.0)
      (c1, e1)
    }}
    2.0  / Constants.SQRT_PI * e
  }

  def erf(x:Double) = {
    if (Math.abs(x) < 1.0) {
      erfseries(x)
    } else {
      // calculate using the complement
      1.0 - Erfc(x)
    }
  }

}
object Erf {
  def apply(x:Double) = new Erf().erf(x)
}

package au.id.cxd.math.function.gamma

import au.id.cxd.math.function.Constants

/**
  * Compute the trigamma function
  *
  * This is copied from GSL psi.c function gsl_sf_psi_1_e line 722.
  */
class Trigamma(val factorialFn: Double => (Double, Double)) extends Polygamma(approxFactorialFn = factorialFn) {



  def op(x:Double) = {
    if (x == 0.0 || x == -1.0 || x == -2.0) {
      throw new IllegalArgumentException(s"x is invalid it cannot be 0, -1.0 or -2.0")
    }
    if (x > 0.0) psiX(1.0, x)
    else if (x > -5,0) {
      // psi.c line 736
      val M = -Math.floor(x)
      val fx = x + M
      val sum = (for (m <- 0 until M.toInt) yield {
        1.0/((x+m)*(x+m))
      }).reduce(_+_)
      val (y,err) = psiX(1, fx)
      val y1 = y + sum
      val err1 = err + M*Constants.DBL_EPSILON*sum
      (y1,err1)
    } else {
      val sin_px = Math.sin(Math.PI*x)
      val d = Math.PI * Math.PI / (sin_px*sin_px)
      val (y, err) = psiX(1.0, 1.0-x)
      val y1 = d - y
      val err1 = err + 2.0*Constants.DBL_EPSILON*d
      (y1, err1)
    }

  }

}

object Trigamma {
  def apply(factorialFn: Double => (Double, Double), x:Double) = new Trigamma(factorialFn).op(x)

  def apply(factorialFn: Double => (Double, Double)) = new Trigamma(factorialFn)
}

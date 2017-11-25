package au.id.cxd.math.function.gamma

import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function.Constants

/**
  * Log factorial function
  *
  * Note that the approximation where applied must accept x > Factorial.MAX_N
  *
  * This approximation function is parameterised due to the circular dependency
  * between LogGamma and PolyGamma both indirectly related dependent through LogFact
  * this occurs in the gsl_library.
  */
class LogFact(approxFn:Double => (Double,Double)) {

  def op(x:Double) = {
    if (x <= Factorial.MAX_N) {
      val y = Math.log (Factorial (x))
      val err = 2.0 * Constants.DBL_EPSILON * Math.abs(y)
      (y, err)
    } else approxFn (x + 1)
  }

}
object LogFact {
  def apply(approxFn:Double=>(Double,Double)) = new LogFact(approxFn)

  def apply(approxFn:Double=>(Double,Double), x:Double) =
    new LogFact(approxFn).op(x)
}

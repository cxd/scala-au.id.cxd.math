package au.id.cxd.math.function.series

import scala.annotation.tailrec

trait ContinuedSeries {

  /**
    * Compute the polynomial
    * given a set of coefficients and a constant value x
    *
    *
    * $$
    * f(x,i) = \begin{cases}
    * c_i + x * f(x,i+1) & \text{where} i < n\\
    * c_i & \text{otherwise}
    * \end{cases}
    *
    * @param coef
    * @param e
    * @return
    */
  def poly(coef: List[Double], e: Double): Double = coef match {
    case c :: Nil => c
    case (c :: cs) => c + e * poly(cs, e)
  }




}

package au.id.cxd.math.function.series

trait ContinuedSeries {

  /**
    * given a set of coefficients and a constant value x
    * compute
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
  def additiveProductFn(coef: List[Double], e: Double): Double = coef match {
    case c :: Nil => c
    case (c :: cs) => c + e * additiveProductFn(cs, e)
  }

}

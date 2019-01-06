package au.id.cxd.math.function.series

/**
  * Implementation of the matlab function polyval
  * which evaluates the coefficients in descending order from nth order to 1.
  * In this function the coefficients are taken in ascending order so that instead of
  *
  * p(x) = p_1 x^n + p_2 x^n-1 + ... + p_n x^1 + p_{n+1}
  *
  * Where the coefficients are specified from highest order (n) to lowest (1)
  *
  * We implement:
  *
  * p(x) = p_0 + p_1 x^1 + p_2 x^2 + p_3 x^3 + ... p_n x^n
  *
  * Or rather
  *
  * p(x) = p_0 + \sum_{i=1}^n p_i x^i
  *
  * This is a convenience function for the underlying polynomial defined in Continued series.
  *
  */
class PolyVal extends ContinuedSeries {
}

object PolyVal {
  def apply(coef:List[Double], x:Double):Double = new PolyVal().poly(coef, x)
}

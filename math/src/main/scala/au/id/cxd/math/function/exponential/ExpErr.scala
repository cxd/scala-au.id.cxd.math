package au.id.cxd.math.function.exponential

import au.id.cxd.math.function.Constants


/**
  * an exponent with an error term
  *
  * as defined in GSL exp.c line 535
  */
class ExpErr {


  def op(x:Double, dx:Double) = {
    val adx = Math.abs(dx)

    if (x+adx > Constants.LOG_DBL_MAX) throw new IllegalArgumentException("Overflow error")
    else if (x - adx < Constants.LOG_DBL_MIN) throw new IllegalArgumentException("Underflow error")

    val ex = Math.exp(x)
    val edx = Math.exp(adx)
    val result_val = ex
    val max = List(Constants.DBL_EPSILON, edx-1.0/edx).max
    val err1 = ex*max
    val err2 = err1 + 2.0*Constants.DBL_EPSILON*Math.abs(result_val)
    (result_val, err2)
  }
}

object ExpErr {
  def apply(x:Double, dx:Double) = new ExpErr().op(x,dx)
}

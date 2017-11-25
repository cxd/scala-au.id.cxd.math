package au.id.cxd.math.function.exponential

import au.id.cxd.math.function.Constants

/**
  * Exponential x with associated absolute error.
  *
  * This is a copy of the GSL library function exponentiation with error estimate
  *
  * gsl_sf_exp_mult_err_e ile exp.c line 229
  */
class ExpMult {

  def op(x:Double, dx:Double, y:Double, dy:Double) = {
    val ay = Math.abs(y)
    if (y == 0.0) {
      (0.0, Math.abs(dy * Math.exp(x)))
    } else if ((x < 0.5*Constants.LOG_DBL_MAX && x > 0.5*Constants.LOG_DBL_MIN) &&
      (ay < 0.8*Constants.SQRT_DBL_MAX && ay > 1.2*Constants.SQRT_DBL_MIN)) {
      val ex = Math.exp(x)
      val y1 = y*ex
      val err = ex * (Math.abs(dy) + Math.abs(y*dx))
      val err1 = err + 2.0 * Constants.DBL_EPSILON*Math.abs(y1)
      (y1,err1)
    } else {
      val ly = Math.log(ay)
      val lnr = x + ly
      if (lnr > Constants.LOG_DBL_MAX - 0.01) {
        throw new IllegalArgumentException(s"x = $x and y = $y value out of range $lnr > ${Constants.LOG_DBL_MAX - 0.01}")
      }
      if (lnr < Constants.LOG_DBL_MIN + 0.01) {
        throw new IllegalArgumentException(s"x = $x y = $y out of range $lnr < ${Constants.LOG_DBL_MIN + 0.01}")
      }
      val sy = Math.signum(y)
      val M = Math.floor(x)
      val N = Math.floor(ly)
      val a = x - M
      val b = ly - N
      val eMN = Math.exp(M+N)
      val eab = Math.exp(a+b)
      val y1 = sy * eMN * eab
      val err = eMN * eab * 2.0*Constants.DBL_EPSILON
      val err1 = err + eMN * eab * Math.abs(dy/y)
      val err2 = err1 + eMN * eab * Math.abs(dx)
      (y1, err2)
    }
  }

}
object ExpMult {
  def apply(x:Double, dx:Double, y:Double, dy:Double) =
    new ExpMult().op(x,dx,y,dy)
}

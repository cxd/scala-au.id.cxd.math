package au.id.cxd.math.probability.random

/**
  * Parameters used in the GSL RBinom algorithm
  * These are computed once and then used during the calculation
  * @param p
  * @param q
  * @param np
  */
case class RBinomParameters(val p:Double, val q:Double, val np:Double ) {

  val s:Double = p / q
  // parameters that do not change.
  val ffm:Double = np + p
  val fm:Double = Math.round(ffm).toDouble
  val xm:Double = fm + 0.5
  val npq:Double = np * q

  // From GSL lin 178.
  /* Compute cumulative area of tri, para, exp tails */

  /* p1: radius of triangle region; since height=1, also: area of region */
  /* p2: p1 + area of parallelogram region */
  /* p3: p2 + area of left tail */
  /* p4: p3 + area of right tail */
  /* pi/p4: probability of i'th area (i=1,2,3,4) */

  /* Note: magic numbers 2.195, 4.6, 0.134, 20.5, 15.3 */
  /* These magic numbers are not adjustable...at least not easily! */
  val p1:Double = Math.floor(2.195 * Math.sqrt(npq) - 4.6 * q) + 0.5

  /* xl, xr: left and right edges of triangle */
  val xl:Double = xm - p1
  val xr:Double = xm + p1

  /* Parameter of exponential tails */
  /* Left tail:  t(x) = c*exp(-lambda_l*[xl - (x+0.5)]) */
  /* Right tail: t(x) = c*exp(-lambda_r*[(x+0.5) - xr]) */
  val c:Double = 0.134 + 20.5 / (15.3 + fm)
  val p2:Double = p1 * (1.0 + c + c)

  val al:Double = (ffm - xl) / (ffm - xl * p)
  val lambda_l :Double= al * (1.0 + 0.5 * al)
  val ar:Double = (xr - ffm) / (xr * q)
  val lambda_r :Double= ar * (1.0 + 0.5 * ar)
  val p3:Double = p2 + c / lambda_l
  val p4:Double = p3 + c / lambda_r
}

case class RBinomOutputs(val ix:Double, val vari:Double, val accept:Double, val u:Double, val v:Double) {}
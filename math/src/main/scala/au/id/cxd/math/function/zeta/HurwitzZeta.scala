package au.id.cxd.math.function.zeta

import au.id.cxd.math.function.Constants

/**
  * Implementation of the hurwitz zeta function based on the GSL implementation.
  *
  * http://mathworld.wolfram.com/HurwitzZetaFunction.html
  *
  * Refer to GSL zeta.c method: gsl_sf_hzeta_e line 714
  */
class HurwitzZeta {

  /** coefficients for Maclaurin summation in hzeta()
    * B_{2j}/(2j)!
    *
    * GSL zeta.c line 531
    *
    */
  private val hzeta = List[Double](
    1.00000000000000000000000000000,
    0.083333333333333333333333333333,
    -0.00138888888888888888888888888889,
    0.000033068783068783068783068783069,
    -8.2671957671957671957671957672e-07,
    2.0876756987868098979210090321e-08,
    -5.2841901386874931848476822022e-10,
    1.3382536530684678832826980975e-11,
    -3.3896802963225828668301953912e-13,
    8.5860620562778445641359054504e-15,
    -2.1748686985580618730415164239e-16,
    5.5090028283602295152026526089e-18,
    -1.3954464685812523340707686264e-19,
    3.5347070396294674716932299778e-21,
    -8.9535174270375468504026113181e-23
  )

  /**
    * implementation of gsl method gsl_sf_hzeta_e line 714 of zeta.c
    *
    * @param s
    * @param q
    * @return
    */
  def op(s: Double, q: Double) = {
    if (s <= 1.0 || q <= 0.0) {
      throw new IllegalArgumentException(s" invalid parameters for s=$s <= 1.0 and q=$q <= 0 ")
    }
    val max_bits = 54.0
    val ln_term0 = -s * Math.log(q)

    if ((s > max_bits && q < 1.0) || (s > 0.5 * max_bits && q < 0.25)) {
      val y = Math.pow(q, -s)
      val err = 2.0 * Constants.DBL_EPSILON * Math.abs(y)
      (y, err)
    } else if (s > 0.5 * max_bits && q < 1.0) {
      val p1 = Math.pow(q, -s)
      val p2 = Math.pow(q / (1.0 + q), s)
      val p3 = Math.pow(q / (2.0 + q), s)
      val y = p1 * (1.0 + p2 + p3)
      val err = Constants.DBL_EPSILON * (0.5 * s + 2.0) * Math.abs(y)
      (y, err)
    } else {
      /* Euler-Maclaurin summation formula
      * [Moshier, p. 400, with several typo corrections]
      */
      val jmax = 12
      val kmax = 10
      val pmax = Math.pow(kmax + q, -s)
      val scp = s
      val pcp = pmax / (kmax + q)
      val ans = pmax * ((kmax + q) / (s - 1.0) + 0.5)
      val ans1 = ans + (for (k <- 0 to kmax) yield {
        Math.pow(k + q, -s)
      }).reduce(_ + _)

      val pairs = hzeta.tail.zipWithIndex
      val result = pairs.foldLeft (ans1, scp, pcp) {
        (accum, pair) =>
          val hz = pair._1
          val j = pair._2 - 1
          val delta = hz * accum._2 * accum._3
          val ans1 = accum._1 + delta
          Math.abs(delta/ans1) < 0.5*Constants.DBL_EPSILON match {
            case true => (ans1, accum._2, accum._3)
            case _ =>
              val scp1 = (accum._2 + 2 * j + 1)*(s+2*j+2)
              val pcp1 = accum._3 / (kmax + q)*(kmax + q)
              (ans1, scp1, pcp1)
          }
      }
      val y = result._1
      val err = 2.0 * (jmax+1.0)*Constants.DBL_EPSILON*Math.abs(y)
      (y,err)
    }
  }

}

object HurwitzZeta {
  def apply(s:Double, q:Double) = new HurwitzZeta().op(s,q)
}

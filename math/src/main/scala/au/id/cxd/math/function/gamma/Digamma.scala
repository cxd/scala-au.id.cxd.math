package au.id.cxd.math.function.gamma

import au.id.cxd.math.function.Constants
import au.id.cxd.math.function.approximate.Chebyshev
import au.id.cxd.math.function.series.ContinuedSeries

/**
  * ##import MathJax
  *
  * Digamma or Psi function
  *
  * https://en.wikipedia.org/wiki/Digamma_function
  *
  * The implementation is a direct copy of the apache commons number Digamma 
  */
class Digamma() extends ContinuedSeries {


  /** Chebyshev fits from SLATEC code for psi(x)
    * *
    * Series for PSI        on the interval  0.         to  1.00000D+00
    * with weighted error   2.03E-17
    * log weighted error  16.69
    * significant figures required  16.39
    * decimal places required  17.37
    * *
    * Series for APSI       on the interval  0.         to  2.50000D-01
    * with weighted error   5.54E-17
    * log weighted error  16.26
    * significant figures required  14.42
    * decimal places required  16.86
    *
    */


  val psics_data: List[Double] = List(
    -.038057080835217922,
    .491415393029387130,
    -.056815747821244730,
    .008357821225914313,
    -.001333232857994342,
    .000220313287069308,
    -.000037040238178456,
    .000006283793654854,
    -.000001071263908506,
    .000000183128394654,
    -.000000031353509361,
    .000000005372808776,
    -.000000000921168141,
    .000000000157981265,
    -.000000000027098646,
    .000000000004648722,
    -.000000000000797527,
    .000000000000136827,
    -.000000000000023475,
    .000000000000004027,
    -.000000000000000691,
    .000000000000000118,
    -.000000000000000020
  )

  val psiSeries = Chebyshev(psics_data, -1, 1)

  val apsics_data: List[Double] = List(
    -.0204749044678185,
    -.0101801271534859,
    .0000559718725387,
    -.0000012917176570,
    .0000000572858606,
    -.0000000038213539,
    .0000000003397434,
    -.0000000000374838,
    .0000000000048990,
    -.0000000000007344,
    .0000000000001233,
    -.0000000000000228,
    .0000000000000045,
    -.0000000000000009,
    .0000000000000002,
    -.0000000000000000
  )
  val aPsiSeries = Chebyshev(apsics_data, -1, 1)

  /** digamma for x both positive and negative; we do both
    * cases here because of the way we use even/odd parts
    * of the function
    *
    * This implementation is a copy of the gsl library psi_x method.
    * psi.c Line 380
    *
    */
  def op(x: Double) = {
    val y = Math.abs(x)
    if (x == 0.0 || x == -1.0 || x == -2.0) {
      throw new IllegalArgumentException(s"x $x is not in valid range for digamma function.")
    }
    if (y >= 2.0) {
      val t = 8.0/(y*y)-1.0
      val y1 = aPsiSeries.op(t)

      if (x < 0.0) {
        val s = Math.sin(Math.PI*x)
        val c = Math.cos(Math.PI*x)
        if (Math.abs(s) < 2.0*Constants.SQRT_DBL_MIN) {
          throw new IllegalArgumentException(s"x $x is not in valid range for digamma function")
        }
        val y2 = Math.log(y) - 0.5/x + y1._1 - Math.PI * c/s
        val err1 = Math.PI * Math.abs(x) * Constants.DBL_EPSILON/(s*s)
        val err2 = err1 + y1._2
        val err = err2 + Constants.DBL_EPSILON*Math.abs(y2)
        (y2,err)
      } else {
        val y2 = Math.log(y) - 0.5/x + y1._1
        val err = y1._2 + Constants.DBL_EPSILON*Math.abs(y2)
        (y2, err)
      }

    } else if (x < -1.0) {
      val v = x+2.0
      val t1 = 1.0/x
      val t2 = 1.0/(x+1.0)
      val t3 = 1.0/v
      val y1 = psiSeries.op(2.0*v-1.0)
      val y2 = -(t1+t2+t3)+y1._1
      val err1 = Constants.DBL_EPSILON*(Math.abs(t1) + Math.abs(x/(t2*t2)) + Math.abs(x/(t3*t3)))
      val err = err1 + y1._2 + Constants.DBL_EPSILON*y2
      (y2,err)
    } else if (x < 0.0) {
      val v = x + 1.0
      val t1 = 1.0/x
      val t2 = 1.0/v
      val y1 = psiSeries.op(2.0*v-1.0)
      val y2 = -(t1+t2)+y1._1
      val err1 = Constants.DBL_EPSILON*(Math.abs(t1)+Math.abs(x/(t2*t2)))
      val err = err1 + y1._2 + Constants.DBL_EPSILON*Math.abs(y2)
      (y2, err)
    } else if (x < 1.0) {
      val t1 = 1.0/x
      val y1 = psiSeries.op(2.0*x-1.0)
      val y2 = -t1 + y1._1
      val err1 = Constants.DBL_EPSILON*t1 + y1._2 + Constants.DBL_EPSILON*Math.abs(y2)
      (y2,err1)
    } else {
      val v = x - 1.0
      psiSeries.op(2.0*v-1.0)
    }
  }
}
object Digamma {
  def apply() = new Digamma()

  def apply(x:Double) = new Digamma().op(x)
}

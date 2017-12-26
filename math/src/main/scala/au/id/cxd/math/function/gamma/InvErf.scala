package au.id.cxd.math.function.gamma

/**
  * implementation of the inverse error function.
  * based on the implementation provided in:
  *
  * "Approximating the erfinv function" by Mike Giles
  *
  * https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
  *
  *
  */
class InvErf {

  /**
    * Pseudo code to compute y = erfinv(x) with p_n(t)
    *
    * Based on the implementation from https://github.com/tensorflow/tensorflow/blob/d27f758308d475ebd76cdfec85dbca12a32cfc3f/tensorflow/compiler/xla/service/elemental_ir_emitter.cc#L318
    *
    * w = log((1-x)*(1+x))
    * if ( w < 5 ) {
    * w = w - 2.5
    * p = sum_{i=1}^n lq[i]*w^i
    * } else {
    * w = sqrt(w) - 3
    * p = sum_{i=1}^n gq[i]*w^i
    * }
    * return p*x
    *
    * @param p
    * @return
    */
  def inverf(p: Double): Double = {
    val lq = List[Double](2.81022636e-08, 3.43273939e-07, -3.5233877e-06,
      -4.39150654e-06, 0.00021858087, -0.00125372503,
      -0.00417768164, 0.246640727, 1.50140941)

    val gq = List[Double](
      -0.000200214257, 0.000100950558, 0.00134934322,
      -0.00367342844, 0.00573950773, -0.0076224613,
      0.00943887047, 1.00167406, 2.83297682
    )

    val w = -1.0 * Math.log((1.0 - p) * (1.0 + p))
    val result = if (w < 5) {
      val w1 = w - 2.5
      val n = lq.length - 1
      (for (i <- 0 to n) yield i).foldLeft(0.0) {
        (p1,i) => lq(i) + p1 * w1
      }
    } else {
      val w1 = Math.sqrt(w) - 3
      val n = gq.length - 1
      (for (i <- 0 to n) yield i).foldLeft(0.0) {
        (p1,i) => gq(i) + p1*w1
      }
    }
    p * result
  }
}
object InvErf {
  def apply(p:Double) = new InvErf().inverf(p)
}
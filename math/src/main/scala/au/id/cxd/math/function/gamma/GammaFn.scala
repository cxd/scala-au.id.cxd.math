package au.id.cxd.math.function.gamma

import scalaz.Memo

/**
  * ##import MathJax
  *
  * The gamma function $\Gamma(a)$ is equivalent to $(a - 1)!$
  *
  *
  * $\Gamma(z)$ = $\int_0&#94;\infty t&#94;{z-1} \exp(-t) dt$
  *
  *
  *
  *
  * It can be approximated using the equation:
  * $$
  * \Gamma(z+1) = \sqrt{2\pi} \times pow \left(  z + \gamma + \frac{1}{2} , {z + \frac{1}{2}} \right) \exp\left[ -\left(z + \gamma + \frac{1}{2} \right) \right] \left[ c_0 + \sum_{i=1}^N \frac{C_i}{z+i} \right]
  * $$
  *
  * Note that the approximation is minimized when $\gamma = 5$ and $N = 6$
  *
  * The implementation of the gamma function is taken from
  * Grant Palmer "Technical Java : Developing Scientific and Engineering Applications", Prentice Hall 2007
  *
  * It is also described in  "Numerical Recipes The Art of Scientific Computing" pp213..215
  *
  * Created by cd on 5/11/14.
  */
class GammaFn {

  /**
    *
    * @param a
    * @return
    */
  def op: (Double => Double) = {
    def innerOp(a: Double) = {
      if (a == 0.0) 0.0
      else {
        val grp1 = a + 0.5
        val grp2 = a + 5.5

        val grp3 = GammaFn.coeffs.head + GammaFn.coeffs.tail.foldLeft((1.0, 0.0)) {
          (pair, c) => {
            val r = c / (a + pair._1)
            (pair._1 + 1.0, r + pair._2)
          }
        }._2
        val gamma = Math.pow(grp2, grp1) * Math.exp(-grp2) * GammaFn.twopi * grp3 / a
        gamma
      }
    }
    Memo.mutableHashMapMemo {
      innerOp
    }
  }

}

object GammaFn {

  val coeffs = List(1.000000000190015, 76.18009172947146, -86.50532032941677,
    24.01409824083091, -1.231739572450155, 0.1208650973866179, -0.539523938495e-5)

  val twopi = Math.sqrt(2.0 * Math.PI)

  def apply(a: Double) = new GammaFn().op(a)
}

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
  * The implementation of the gamma function is derived from the GSL implementation of the Log Gamma function
  * in special/gamma.c gsl_sf_lngamma_e
  *
  *
  * Created by cd on 5/11/14.
  */
class GammaFn {

  /**
    *
    * @param a
    * @return
    */
  def op(a: Double): Double =
    Math.exp(LogGammaFn(a)._1)


}

object GammaFn {
  def apply(a: Double) = new GammaFn().op(a)
}

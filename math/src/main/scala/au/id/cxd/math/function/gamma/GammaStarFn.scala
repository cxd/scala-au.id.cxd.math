package au.id.cxd.math.function.gamma

/**
  * # import MathJax
  *
  * approximate implementation of the regulated gamma function $\Gamma&#94;*$
  * based on the notes in GSL where
  *
  * $$
  * \Gamma&#94;*(x) = \Gamma(x) / \left( \sqrt{2\pi}x&#94;{(x-1/2)}\exp(-x) \right)
  * $$
  *
  *
  */
class GammaStarFn {

  def op(x:Double) = {
    GammaFn(x) / ( Math.sqrt(2.0*Math.PI) * Math.pow(x, (x-0.5)) * Math.exp(-x)  )
  }
}
object GammaStarFn {
  def apply(x:Double) = new GammaStarFn().op(x)
}
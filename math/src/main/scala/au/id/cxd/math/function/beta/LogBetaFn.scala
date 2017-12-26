package au.id.cxd.math.function.beta

import au.id.cxd.math.function.gamma.{GammaFn, LogGammaFn}

/**
  * #import MathJax
  *
  * Log beta is implemented as the log of the beta function.
  *
  * So for example if beta is:
  *
  *
  * $$
  * B(a,b) = \Gamma(a)\Gamma(b) / \Gamma(a + b)
  * $$
  *
  * Then log beta is
  *
  * $$
  * \log B(a,b) = \log \Gamma(a) + \log \Gamma(b) - \log \Gamma(a + b)
  * $$
  *
  * log gamma is provided by the log gamma function.
  *
  */
class LogBetaFn {

  def op(a:Double, b:Double) : Double =
    LogGammaFn(a)._1 + LogGammaFn(b)._1 - LogGammaFn(a + b)._1
}
object LogBetaFn {
  def apply(a:Double, b:Double):Double = new LogBetaFn().op(a,b)
}


package au.id.cxd.math.probability.continuous

import au.id.cxd.math.function.gamma.LogGammaFn

/**
  * ##import MathJax
  *
  * Chi Square distribution is a special case of the [[Gamma]] distribution
  * with the parameter "df" for degrees of freedom (or $k$)
  *
  * This results in a gamma distribution with $\alpha = df/2$ and $\beta = 2$
  *
  *
  *
  * Created by cd on 5/11/14.
  */
class ChiSquare(val df: Double) extends Gamma(df / 2.0, 2.0) {

  /**
    * implementation of chisq pdf derived from gsl_ran_chisq_pdf file randist/chisq.c line:40
    * @param y
    * @return
    */
  override def pdf(y: Double): Double = {
    if (y < 0.0) 0.0
    else if (df == 2.0) {
      Math.exp(-y/2.0)/2.0
    } else {
      val lng = LogGammaFn(df/2.0)._1
      val p = Math.exp( (df/2.0 - 1.0) * Math.log(y/2.0) - y/2.0 - lng ) / 2.0
      p
    }
  }

}

object ChiSquare {
  def apply(df: Double) = new ChiSquare(df)
}

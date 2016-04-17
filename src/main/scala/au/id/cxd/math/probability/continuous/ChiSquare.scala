package au.id.cxd.math.probability.continuous

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
}

object ChiSquare {
  def apply(df: Double) = new ChiSquare(df)
}

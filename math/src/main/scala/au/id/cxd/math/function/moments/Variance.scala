package au.id.cxd.math.function.moments

/**
  * empirical variance assuming normality
  * 1/n * \sum_{i=1}^n (X - \bar{X})^2
  */
class Variance {

  def op(series:Seq[Double]): Double = {
    val n = series.length
    val mu = Mean(series)
    val sqErr = series.reduce((b,a) => b + Math.pow(a - mu, 2.0))
    val c = 1.0 / n
    c * sqErr
  }
}
object Variance {
  def apply(series:Seq[Double]) :Double = new Variance().op(series)
}

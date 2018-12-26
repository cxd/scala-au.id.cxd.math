package au.id.cxd.math.function.moments

/**
  * Empirical Variance assuming normality scaled by 1/(n-1) instead of 1/n
  */
class VarianceUnbiased {
  def op(series:Seq[Double]): Double = {
    val n = series.length
    val mu = Mean(series)
    val sqErr = series.reduce((b,a) => b + Math.pow(a - mu, 2.0))
    val c = 1.0 / (n-1.0)
    c * sqErr
  }
}
object VarianceUnbiased {
  def apply(series:Seq[Double]):Double = new VarianceUnbiased().op(series)
}
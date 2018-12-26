package au.id.cxd.math.function.moments

/**
  * empirical kurtosis
  * https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
  *
  * k = \frac{\sum_{i=1}^N (X-\bar{X})^4 / N}{s^4)
  *
  */
class Kurtosis {
  def op(series:Seq[Double]):Double = {
    val n = series.length
    val mu = Mean(series)
    val s = Math.sqrt(Variance(series))
    val fourth = series.reduce((b,a) => b + Math.pow(a - mu, 4.0) / n)
    fourth / Math.pow(s, 4.0)
  }
}
object Kurtosis {
  def apply(series:Seq[Double]):Double = new Kurtosis().op(series)
}

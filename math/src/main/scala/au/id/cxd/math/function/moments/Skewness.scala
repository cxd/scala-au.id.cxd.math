package au.id.cxd.math.function.moments

/**
  * empirical skewness assuming normality
  * https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
  * adjusted Fisher-Pearson coefficient of skewness
  * G = \frac{\sqrt{N(N-1)}}{N-2} \frac{\sum_{i=1}^N(X_i - \bar{X})^3/N}{s^3}
  *
  * For standard normal distribution skewness should be 0.
  */
class Skewness {
  def op(series:Seq[Double]):Double = {
    val n = series.length
    val mu = Mean(series)
    val s = Math.sqrt(Variance(series))
    val all = Seq(Math.pow(series.head - mu, 3.0)/n) ++ series.tail
    val third = all.reduce((b,a) => b + Math.pow(a - mu, 3.0) / n)
    val scale = Math.sqrt(n * (n-1)) / (n-2)
    scale * (third / Math.pow(s, 3.0))
  }
}
object Skewness {
  def apply(series:Seq[Double]):Double = new Skewness().op(series)
}

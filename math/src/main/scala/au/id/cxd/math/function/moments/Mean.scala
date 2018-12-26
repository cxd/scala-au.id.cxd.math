package au.id.cxd.math.function.moments

/*
the empirical mean of a sequence assuming normality
 */
class Mean {

  /**
    * compute the empirical mean of a sequence
    *
    * @param series
    * @return
    */
  def op(series: Seq[Double]): Double = {
    val n = series.length
    val total = series.reduce(_ + _)
    total / n
  }
}

object Mean {
  def apply(series:Seq[Double]):Double = new Mean().op(series)
}

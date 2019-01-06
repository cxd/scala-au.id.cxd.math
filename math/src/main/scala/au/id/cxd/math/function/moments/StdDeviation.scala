package au.id.cxd.math.function.moments

/**
  * a convenience function for standard deviation when variance is not directly required.
  */
class StdDeviation extends Variance {

  override def op(series:Seq[Double]): Double = {
    val variance = super.op(series)
    Math.sqrt(variance)
  }
}

object StdDeviation {
  def apply(series:Seq[Double]) = new StdDeviation().op(series)
}

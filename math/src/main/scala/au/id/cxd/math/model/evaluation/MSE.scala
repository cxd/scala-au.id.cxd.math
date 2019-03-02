package au.id.cxd.math.model.evaluation

/**
  * traditional Mean Square Error measure
  *
  * MSE = 1/N \sum_{i=1} (O_i - P_i)^2
  */
class MSE(val obs:Seq[Double], val pred:Seq[Double]) {

  def op():Double = {
    val n = obs.size
    val ss = obs.zip(pred).foldLeft(0.0) {
      (acc, pair) => acc + Math.pow(pair._1 - pair._2, 2.0)
    }
    ss / n
  }
}
object MSE {
  def apply(obs:Seq[Double], pred:Seq[Double]):Double =
    new MSE(obs, pred).op
}
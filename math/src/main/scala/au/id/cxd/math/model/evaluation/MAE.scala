package au.id.cxd.math.model.evaluation

/**
  * Mean Absolute Error
  *
  * MAE = 1/N \sum_{i=1}|O_i - P_i|
  *
  * @param obs
  * @param pred
  */
class MAE(val obs:Seq[Double], val pred:Seq[Double]) {

  def op():Double = {
    val n = obs.size
    val err = obs.zip(pred).foldLeft(0.0) {
      (acc, pair) => acc + Math.abs(pair._1 - pair._2)
    }
    err / n
  }
}
object MAE {
  def apply(obs:Seq[Double], pred:Seq[Double]):Double =
    new MAE(obs, pred).op
}
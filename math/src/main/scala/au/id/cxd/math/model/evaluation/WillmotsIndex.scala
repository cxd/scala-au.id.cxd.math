package au.id.cxd.math.model.evaluation

import au.id.cxd.math.function.moments.Mean

/**
  * Calculate wilmots index for the power j.
  * Defaults to j=2.
  *
  *
  * d_j = 1.0 - \frac{\sum_{i=1}|O_i - P_i|^j}{\sum_{i=1}(|P_i-\bar{O}| + |O_i - \bar{O}|)^j}
 *
  * @param j
  * @param obs
  * @param pred
  */
class WillmotsIndex(val j:Double=2.0, val obs:Seq[Double], val pred:Seq[Double]) {

  def op():Double = {
    val obs_mean = Mean(obs)
    val delta = obs.zip(pred).foldLeft(0.0) {
      (acc, pair) =>
        acc + Math.pow(Math.abs(pair._1 - pair._2), j)
    }
    val error = obs.zip(pred).foldLeft(0.0) {
      (acc, pair) =>
        acc + Math.pow(Math.abs(pair._2 - obs_mean) + Math.abs(pair._1 - obs_mean), j)
    }
    1.0 - delta / error
  }

}
object WillmotsIndex {
  def apply(j:Double=2.0, obs:Seq[Double], pred:Seq[Double]):Double =
    new WillmotsIndex(j,obs,pred).op()
}

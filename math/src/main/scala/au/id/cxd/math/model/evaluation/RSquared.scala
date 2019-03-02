package au.id.cxd.math.model.evaluation

import au.id.cxd.math.function.moments.Mean

/**
  * calculate R-squared metric for distance as the correlation coefficient.
  *
  * Note howevber R^2 has the shortcoming of being influenced by extreme values
  * in variation given its use of the squared error.
  *
  *
  */
class RSquared(val obs:Seq[Double], val pred:Seq[Double]) {

  def op():Double = {
    val obs_mean = Mean(obs)
    val pred_mean = Mean(pred)
    val obs_ss = obs.foldLeft(0.0)((acc, o) => acc + (o - obs_mean)*(o - obs_mean))
    val pred_ss = pred.foldLeft(0.0)((acc,p) => acc + (p - pred_mean)*(p-pred_mean))
    val joint_ss = obs.zip(pred).foldLeft(0.0) {
      (acc, pair) =>
        acc + (pair._1 - obs_mean)*(pair._2 - pred_mean)
    }
    joint_ss / (obs_ss * pred_ss)
  }
}


object RSquared {

  /**
    * calculate R^2
    * @param obs - observations
    * @param pred - predicted values.
    * @return
    */
  def apply(obs:Seq[Double], pred:Seq[Double]):Double =
    new RSquared(obs, pred).op()

}

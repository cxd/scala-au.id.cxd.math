package au.id.cxd.math.model.evaluation

import au.id.cxd.math.function.moments.Mean


/**
  * Calculate the generalised coefficient of efficiency
  *
  * j defaults to 2
  *
  * E_j = 1.0 - \frac{\sum_{i=1}|O_i - P_i|^j}{\sum_{i=1}|O_i - \bar{O}|^j}
  *
  */
class Efficiency(val j:Double=2.0, val obs:Seq[Double], val pred:Seq[Double]) {

  def op():Double = {
    val obs_mean = Mean(obs)
    val delta = obs.zip(pred).foldLeft(0.0) {
      (acc, pair) => acc + Math.pow(Math.abs(pair._1 - pair._2),j)
    }
    val obs_ss = obs.foldLeft(0.0) {
      (acc, o) => acc + Math.pow(Math.abs(o - obs_mean), j)
    }
    1.0 - delta/obs_ss
  }
}
object Efficiency {
  def apply(j:Double=2.0, obs:Seq[Double], pred:Seq[Double]):Double =
    new Efficiency(j,obs,pred).op
}

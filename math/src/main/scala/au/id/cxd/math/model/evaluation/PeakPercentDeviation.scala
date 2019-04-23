package au.id.cxd.math.model.evaluation

/**
  * compute peak percentage deviation.
  *
  * @param obs
  * @param sim
  */
class PeakPercentDeviation(val obs:Seq[Double], val sim:Seq[Double]) {

  def op():Double = {
    val maxObs = obs.max
    val maxSim = sim.max
    val pdv = 100.0 * (maxSim - maxObs) / maxObs
    pdv
  }

}

object PeakPercentDeviation {
  def apply(obs:Seq[Double], sim:Seq[Double]):Double =
    new PeakPercentDeviation(obs, sim).op()
}

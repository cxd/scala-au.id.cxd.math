package au.id.cxd.math.model.particle.mcmc

/**
  * A state that is threaded through each step of the sampling computation.
  */
trait State {

  /**
    * The step at which the state was observed.
    */
  val step:Int

}

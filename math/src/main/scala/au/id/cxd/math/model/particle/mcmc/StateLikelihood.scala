package au.id.cxd.math.model.particle.mcmc

trait StateLikelihood {


  /**
    * default implementation of likelihood of the state.
    * @param state
    * @return
    */
  def likelihood(state:State) = Math.exp(logLikelihood(state))

  /**
    * determine the log likelihood of the state.
    * @param state
    * @return
    */
  def logLikelihood(state:State):Double
}

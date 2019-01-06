package au.id.cxd.math.model.particle.mcmc

/**
  * A proposal computes the next state given the previous state and makes use of a proposal distribution.
  * When it draws new samples from the proposal distribution it also must determine the probability
  * of the samples
  *
  * Probability of previous given new p(x_1|x_2)
  * Probability of new given previous p(x_2|x_1)
  *
  * This is then used as a ratio
  *
  * q(x_1|x_2) / q(x_2|x_2)
  *
  * As a consequence it is assumed the proposal can calculate the probability of a state.
  *
  */
trait Proposal {

  /**
    * draw a new proposal given the previous state
    * @param prior
    * @return
    */
  def propose (step:Int, prior:State, likelihood: StateLikelihood):(State, Double, Double)

}

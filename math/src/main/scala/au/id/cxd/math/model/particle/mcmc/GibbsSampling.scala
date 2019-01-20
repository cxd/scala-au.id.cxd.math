package au.id.cxd.math.model.particle.mcmc

import au.id.cxd.math.probability.continuous.Normal
import au.id.cxd.math.probability.random.RNormal

/**
  * initialise with
  *
  * @param initialState
  * The initial state representation.
  * @param proposal
  * The proposal generator for new states.
  * @param logLikelihoodFn
  * The log likelihood function p(s|X)
  * the likelihood of the state given the data.
  */
class GibbsSampling(override val initialState: State,
                    override val proposal: Proposal,
                    likelihood: StateLikelihood) extends McmcSampling {

  /**
    * mutable values storing counts of accepted states over the iteration.
    * it could be possible to trace these over iterations instead.
    * maybe change these to a mutable collection over time if desired.
    */
  var accepted: Double = 0.0
  var total: Double = 0.0

  /**
    * random sample for acceptance test.
    */
  val rnorm = RNormal()

  /**
    * Take a single step and determine which state to retain as the next state.
    *
    * @param i
    * @param prev
    * @return
    */
  override def step(i: Int, prev: State): State = {
    val (nextState, qratio) = proposal.propose(i, prev, likelihood)
    val prevLogP = likelihood.logLikelihood(prev)
    val nextLogP = likelihood.logLikelihood(nextState)
    val scores = Array(1.0, qratio * Math.exp(nextLogP - prevLogP))
    val alpha = scores.min
    val r = rnorm.draw()
    // acceptance test
    if (r < alpha) {
      //
      total = total + 1.0
      accepted = accepted + 1.0
      nextState
    } else {
      total = total + 1.0
      prev
    }
  }

}

object GibbsSampling {

  def apply(initialState: State, proposal: Proposal, likelihood: StateLikelihood) =
    new GibbsSampling(initialState, proposal, likelihood)
}
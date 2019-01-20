import au.id.cxd.math.model.particle.mcmc.{GibbsSampling, Proposal, State, StateLikelihood}
import au.id.cxd.math.probability.continuous.Gamma
import au.id.cxd.math.probability.random.{RNormal, RPoisson}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable

/**
  * A simulation example using MCMC based on the example given in chapter 15 of Numerical recipes.
  * *
  * Investigation of a random poisson process with two different states for parameter lambda.
  *
  * The problem is given as
  *
  * At the beginning of an experiment events occur Poisson randomly with
  * a mean rate lambda_1, but only every k_1th event is recorded.
  * Then at time t_c the mean rate changes to lambda_2 and only every k_2th event is recorded.
  * We are given times t_0, ... , t_N-1 of the N recorded events.
  * We want to find unknown parameters lambda_1, k_1, lambda_2, k_2 and t_c.
  *
  *
  **/

// The data that is observed prior to the simulations. We are seeking to discover the parameters for
// lambda1, k1, and after time t, lambda2 k2
// The data set in this case will be generated to simulate the observed data before we run the MCMC experiment.
// In our artificial data set we wil have a distribution where the number of events occur where lambda = 4 and we will record
// only every 2nd event.

val randP1 = RPoisson(4.0)
val samples1:Seq[Double] = for (i <- 1 to 200; if i % 2 == 0) yield randP1.draw()

// for the second set of data the lambda will change to 10
val randP2 = RPoisson(10.0)
val samples2:Seq[Double] = for (i <- 1 to 200; if i % 2 == 0) yield randP2.draw()

val allsamples = samples1 ++ samples2

/**
  * The state that will store parameters during the learning process.
  *
  * @param step
  * @param lambda1
  * @param lambda2
  * @param k1
  * @param k2
  * @param t
  */
case class ExampleState(override val step: Int = 0,
                        val lambda1: Double = 0.0,
                        val lambda2: Double = 0.0,
                        val k1: Double = 0.0,
                        val k2: Double = 0.0,
                        val t: Double = 0.0) extends State {


}


case class ExampleProposal(val logstep: Double = 0.01) extends Proposal {

  val gauss = RNormal()

  /**
    * draw a new proposal given the previous state
    *
    * @param prior
    * @return
    */
  override def propose(step: Int, prior: State, likelihood: StateLikelihood): (State, Double) = {
    val r = gauss.draw()
    val prev = prior.asInstanceOf[ExampleState]
    if (r < 0.9) {
      // steps holding the ks constant.
      val lam1 = prev.lambda1 * Math.exp(logstep * gauss.draw())
      val lam2 = prev.lambda2 * Math.exp(logstep * gauss.draw())
      val tc2 = prev.t * Math.exp(logstep * gauss.draw())
      val s = ExampleState(step, lam1, lam2, prev.k1, prev.k2, tc2)
      val qratio = (lam1 / prev.lambda1) * (lam2 / prev.lambda2) * (tc2 / prev.t)
      (s, qratio)
    } else {
      // cases modifying the k parameters
      val r2 = gauss.draw()
      val k1 = if (prev.k1 > 1.0) {
        if (r2 < 0.5) prev.k1
        else if (r2 < 0.75) prev.k1 + 1.0
        else prev.k1 - 1.0
      } else if (r2 < 0.75) prev.k1
      else prev.k1 + 1.0

      val lam1 = k1 * prev.lambda1 / prev.k1

      val r3 = gauss.draw()

      val k2 = if (prev.k2 > 1.0) {
        if (r3 < 0.5) prev.k2
        else if (r3 < 0.75) prev.k2 + 1.0
        else prev.k2 - 1.0
      } else if (r3 < 0.75) prev.k2
      else prev.k2 + 1.0

      val lam2 = k2 * prev.lambda2 / prev.k2

      val tc2 = prev.t
      val qratio = 1.0

      val s = ExampleState(step, lam1, lam2, k1, k2, tc2)
      (s, qratio)
    }

  }

}

/**
  * geenrate empirical estimates of probability for simulated states based on the
  * data from initial experiments.
  *
  * This example follows the iterative mutable example from ch15 in Numerical recipes.
  *
  * @param data
  */
case class ExampleLikelihood(val observations: Seq[Double]) extends StateLikelihood {

  val data: Seq[Double] = observations.sorted

  val stau: ArrayBuffer[Double] = ArrayBuffer[Double]()

  val slogtau: ArrayBuffer[Double] = ArrayBuffer[Double]()

  stau += 0.0
  slogtau += 0.0

  for (i <- 1 until data.length) {
    stau :+ data(i) - data(0)
    slogtau :+ slogtau(i - 1) + Math.log(data(i) - data(i - 1))
  }

  /**
    * search in the ordered data for position of x
    *
    * @param x
    * @param data
    * @return
    */
  def bisectSearch(x: Double, data: Seq[Double]): (Int, Int) = {

    @tailrec
    def inner(ilo: Int, ihi: Int): (Int, Int) = {
      if (ihi - ilo <= 1) (ilo, ihi)
      else {
        val i = (ihi + ilo) / 2
        val (ilo1, ihi1) = if (x > data(i)) (i, ihi)
        else (ilo, i)
        inner(ilo1, ihi1)
      }

    }

    inner(0, data.length - 1)
  }

  /**
    * determine the log likelihood of the state.
    *
    * @param state
    * @return
    */
  override def logLikelihood(state: State): Double = {
    val curState: ExampleState = state.asInstanceOf[ExampleState]
    val tc = curState.t

    // find index of tc in data.
    val (ilo, ihi) = bisectSearch(tc, data)
    val n1 = ihi
    val n2 = data.length - 1 - ihi
    val st1 = stau(ihi)
    val st2 = stau(data.length - 1) - st1
    val stl1 = slogtau(ihi)
    val stl2 = slogtau(data.length - 1) - stl1
    // waiting time  to next event is modelled as a gamma distribution Gamma(k,lambda)
    // p(tau|k,lambda)
    val gamma1 = Gamma(alpha = curState.k1)(beta = curState.lambda1)
    val p1 = gamma1.pdf(n1)

    val gamma2 = Gamma(alpha = curState.k2)(beta = curState.lambda2)
    val p2 = gamma2.pdf(n2)

    // joint likelihood

    // P(D|x)
    val ans = Math.log(p1) + Math.log(p2)
    ans
  }

}

val likelihood:StateLikelihood = ExampleLikelihood(allsamples)
val proposal:Proposal = ExampleProposal()
val initialState:State = ExampleState(0, lambda1=1.0, lambda2=3.0, k1=1.0, k2=1.0, t=200.0)

val sampler = GibbsSampling(initialState: State, proposal: Proposal, likelihood: StateLikelihood)


def runsampler[T](max:Int, i:Int, sampler:GibbsSampling, prevState:State, seed:T)(accumFn:(T,State) => T) : (Int, GibbsSampling, State, T) = {
  if (i > max) (i, sampler, prevState, seed)
  else {
    val state = sampler.step(i,prevState)
    val seed1 = accumFn(seed, state)
    runsampler(max, i+1, sampler, state, seed1)(accumFn)
  }
}


// burnin for first 1000 iterations.
val (steps1, sampler1, lastState, accum1) = runsampler(1000, 1, sampler, initialState, 0) ((accum,st) => 0)

// collect data for the next 1000 samples.
val (steps2, sampler2, lastState2, accum2) = runsampler(1000, 1, sampler, lastState, Seq[ExampleState]()) {
  (accum, state) => accum :+ state.asInstanceOf[ExampleState]
}

val k1 = accum2.map(s => s.k1)
val k2 = accum2.map(s => s.k2)

// TODO: graph k1 and k2

println(k1)
println(k2)




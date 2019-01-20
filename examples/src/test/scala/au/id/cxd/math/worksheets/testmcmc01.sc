import au.id.cxd.math.example.model.mcmc.nr.{ExampleLikelihood, ExampleProposal, ExampleState}
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

val allsamples = (samples1 ++ samples2).filter { x => x > 0.0 }


val likelihood:StateLikelihood = ExampleLikelihood(allsamples)

val proposal:Proposal = ExampleProposal()
val initialState:State = ExampleState(0, lambda1=1.0, lambda2=3.0, k1=1.0, k2=1.0, t=10.0)

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
val lambda1 = accum2.map(s => s.lambda1)
val k2 = accum2.map(s => s.k2)
val lambda2 = accum2.map(s => s.lambda2)

println("k1")
println(k1)

println("lambda1")
println(lambda1)

println("k2")
println(k2)

println("lambda2")
println(lambda2)





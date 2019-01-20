package au.id.cxd.math.model.particle.mcmc

import scala.annotation.tailrec

trait McmcSampling {

  val initialState:State

  val proposal:Proposal

  /**
    * take a sampling step.
    * @param i
    * @param prev
    * @return
    */
  def step(i:Int, prev:State):State

  /**
    * Sample n times producing the final state
    * fold a starting seed state through the iteration via an accumulator function.
    * @param n
    * @param initial
    * @return final state and accumulated data
    */
  def run[T](n:Int, initial:State, seed:T)(accumFn:(T,State) => T):(State,T) = {
    @tailrec
    def innerSample(i:Int, prev:State, seed:T, accumFn:(T,State) => T):(State,T) =
      if (i > n) (prev, seed)
      else {
        val nextSt = step(i,prev)
        val seed2 = accumFn(seed, nextSt)
        innerSample(i+1, nextSt, seed2, accumFn)
      }
    val seed1 = accumFn(seed, initial)
    innerSample(0,initial, seed1, accumFn)
  }

}

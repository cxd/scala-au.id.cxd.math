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
    * @param n
    * @param initial
    * @return final state.
    */
  def run(n:Int, initial:State):State = {
    @tailrec
    def innerSample(i:Int, prev:State):State =
      if (i > n) prev
      else innerSample(i+1, step(i,prev))
    innerSample(0,initial)
  }

}

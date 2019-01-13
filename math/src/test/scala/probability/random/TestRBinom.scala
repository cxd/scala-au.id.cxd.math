package probability.random

import au.id.cxd.math.probability.random.{RApproxBinom, RBinom}
import org.scalatest.{FlatSpec, Matchers}

class TestRBinom extends FlatSpec with Matchers {

  // TODO: work on debugging the RBinom distribution
  "rbinom" should "Draw random samples from distribution" in {
    val binom = RBinom(n=5, p=0.3)
    val samples = binom.draw(100)
    println(samples)
    // note that larger samples of binomial should have a distribution that is approximately normal
  }

  "rapproxbinom" should "Draw random samples from distribution" in {
    val binom = RApproxBinom(n=5, p=0.3)
    val samples = binom.draw(100)
    println(samples)
    // note that larger samples of binomial should have a distribution that is approximately normal
  }

}

package probability.random

import au.id.cxd.math.probability.random.{RApproxBinom, RBinom}
import org.scalatest.{FlatSpec, Matchers}

class TestRBinom extends FlatSpec with Matchers {

  "rbinom" should "Draw random samples from distribution" in {
    val binom = RBinom(n=5, p=0.3)
    val samples = binom.draw(1000)
    println(samples)
    // note that larger samples of binomial should have a distribution that is approximately normal
  }

  "rapproxbinom" should "Draw random samples from distribution" in {
    val binom = RApproxBinom(n=5, p=0.3)
    val samples = binom.draw(100)
    println(samples)
    // note that larger samples of binomial should have a distribution that is approximately normal
  }

  "rbinom" should "Draw random samples from distribution with large n" in {
    val binom = RBinom(n=25, p=0.3)
    val samples = binom.draw(100)
    println(samples)
    // note that larger samples of binomial should have a distribution that is approximately normal
  }

}

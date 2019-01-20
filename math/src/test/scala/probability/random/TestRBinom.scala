package probability.random

import au.id.cxd.math.probability.analysis.AndersonDarlingTest
import au.id.cxd.math.probability.discrete.Binomial
import au.id.cxd.math.probability.random.{RApproxBinom, RBinom}
import org.scalatest.{FlatSpec, Matchers}

// TODO: need a goodness of fit test to test membership of the binomial function.
// as it stands the results can be visually compared against generating the same distributions in R.
// the vectors can be cut and paste into R and visually compared.

class TestRBinom extends FlatSpec with Matchers {

  def makeCDF(n:Double, p:Double):Double => Double = {
    val binom = Binomial(n)(p)
    (x:Double) => binom.cdf(for (i <- 0 to x.toInt) yield i.toDouble)
  }

  "rbinom" should "Draw random samples from distribution" in {
    val binom = RBinom(n=5, p=0.3)
    val samples = binom.draw(100)

    val cdf = makeCDF(5,0.3)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

    println(samples)
  }


  "rbinom" should "Draw random samples from distribution with high p" in {
    val binom = RBinom(n=5, p=0.8)
    val samples = binom.draw(100)
    println(samples)

    val cdf = makeCDF(5,0.8)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

  }

  "rapproxbinom" should "Draw random samples from distribution" in {
    val binom = RApproxBinom(n=5, p=0.3)
    val samples = binom.draw(100)
    println(samples)

    val cdf = makeCDF(5,0.3)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)


  }

  "rbinom" should "Draw random samples from distribution with large n" in {
    val binom = RBinom(n=25, p=0.3)
    val samples = binom.draw(100)
    println(samples)

    val cdf = makeCDF(25,0.3)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)


  }

  "rbinom" should "Draw random samples from distribution with large n and high p" in {
    val binom = RBinom(n=25, p=0.8)
    val samples = binom.draw(100)
    println(samples)

    val cdf = makeCDF(25,0.8)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

  }

}

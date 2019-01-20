package probability.random

import au.id.cxd.math.probability.analysis.AndersonDarlingTest
import au.id.cxd.math.probability.discrete.{Binomial, Poisson}
import au.id.cxd.math.probability.random.{RBinom, RPoisson}
import org.scalatest.{FlatSpec, Matchers}

class TestRPoisson extends FlatSpec with Matchers {


  def makeCDF(lambda:Double):Double => Double = {
    val dist = Poisson(lambda)
    (x:Double) => dist.cdf(for (i <- 0 to x.toInt) yield i.toDouble)
  }

  "rpoisson" should "Draw random samples from distribution lambda=1.0" in {
    val lambda = 1.0
    val rdist = RPoisson(lambda)
    val samples = rdist.draw(100)

    val cdf = makeCDF(lambda)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

    println(samples)
  }


  "rpoisson" should "Draw random samples from distribution lambda=4.0" in {
    val lambda = 4.0
    val rdist = RPoisson(lambda)
    val samples = rdist.draw(100)

    val cdf = makeCDF(lambda)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

    println(samples)
  }


  "rpoisson" should "Draw random samples from distribution lambda=10.0" in {
    val lambda = 10.0
    val rdist = RPoisson(lambda)
    val samples = rdist.draw(100)

    val cdf = makeCDF(lambda)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

    println(samples)
  }


  "rpoisson" should "Draw random samples from distribution lambda=12.0" in {
    val lambda = 12.0
    val rdist = RPoisson(lambda)
    val samples = rdist.draw(100)

    val cdf = makeCDF(lambda)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)

    println(samples)
  }
}

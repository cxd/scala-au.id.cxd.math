package probability.random

import au.id.cxd.math.probability.analysis.AndersonDarlingTest
import au.id.cxd.math.probability.continuous.Gamma
import au.id.cxd.math.probability.random.RGamma
import org.scalatest.{FlatSpec, Matchers}

class TestRGamma extends FlatSpec with Matchers {

  "rgamma" should "produce variates in gamma distribution" in {
    val alpha = 3.0
    val beta = 2.0
    val gamma = Gamma(alpha, beta)
    val rgamma = RGamma(alpha,beta)


    val samples = rgamma.draw(100)

    println(samples)

    // we don't yet have a test for gamma distribution goodness of fit.
    // visually inspecting the samples produced from above and
    // using rgamma(100, alpha, beta) in R produces similar density plots.
    // There is a test proposed in https://doi.org/10.1016/j.spl.2014.10.001
    // which could be used to test for membership of gamma distribution.
  }

}

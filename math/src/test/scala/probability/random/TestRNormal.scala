package probability.random

import au.id.cxd.math.probability.analysis.AndersonDarling
import au.id.cxd.math.probability.continuous.Normal
import au.id.cxd.math.probability.random.RNormal
import org.scalatest.{FlatSpec, Matchers}

class TestRNormal extends FlatSpec with Matchers {

  "unit circle" should "be produced by RNorm" in {
    val rnorm = RNormal()
    val unit = rnorm.unitCircle
    println(unit)


  }

  "random normal draws" should "be from normal dist N(0,1)" in {
    val rnorm = RNormal()
    val samples = rnorm.draw(100)
    println(samples)

    // perform test for normality eg: Shapiro-Wilks or Anderson Darling test.
    val norm = Normal(mu=0.0)(1.0)
    val cdf = (x:Double) => norm.cdf(x)
    val test = AndersonDarling(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)
    result.reject should be (false)
  }


}

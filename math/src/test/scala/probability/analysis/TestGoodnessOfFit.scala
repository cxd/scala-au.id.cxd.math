package probability.analysis

import au.id.cxd.math.probability.analysis.{AndersonDarlingTest, JarqueBeraTest, ShapiroWilksTest}
import au.id.cxd.math.probability.continuous.Normal
import org.scalatest.{FlatSpec, Matchers}

class TestGoodnessOfFit extends FlatSpec
  with Matchers
  with TestData {


  "AndersonDarling weibull" should "suggest evidence to reject ad.test null hypothesis" in {
    val samples = weibullTest
    val norm = Normal(mu=0.0)(1.0)
    val cdf = (x:Double) => norm.cdf(x)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(0.05)
    println(result.toString)
    // should reject the null hypothesis
    result.reject should be (true)
  }

  "AndersonDarling normtest" should "not suggest evidence to reject ad.test null hypothesis" in {
    val samples = normTest
    val norm = Normal(mu=0.0)(1.0)
    val cdf = (x:Double) => norm.cdf(x)
    val test = AndersonDarlingTest(samples, cdf)
    val result = test.test(alpha=0.05)
    println(result.toString)
    result.reject should be(false)
  }



  "JarqueBera weibull" should "suggest evidence to reject ad.test null hypothesis" in {
    val samples = weibullTest
    val test = JarqueBeraTest(samples)
    val result = test.test(0.05)
    println(result.toString)
    // should reject the null hypothesis
    result.reject should be (true)
  }

  "JarqueBera normtest" should "not suggest evidence to reject ad.test null hypothesis" in {
    val samples = normTest
    val test = JarqueBeraTest(samples)
    val result = test.test(alpha=0.05)
    println(result.toString)
    result.reject should be(false)
  }

  "ShapiroWilks wiebull" should "suggest evidence to reject null hypothesis" in {
    val samples = weibullTest
    val test = ShapiroWilksTest(samples)
    val result = test.test(0.05)
    println(result.toString)
    // should reject the null hypothesis
    result.reject should be (true)
  }

  "ShapiroWilks normtest" should "not suggest evidence to reject null hypothesis" in {
    val samples = normTest
    val test = ShapiroWilksTest(samples)
    val result = test.test(0.05)
    println(result.toString)
    // should reject the null hypothesis
    result.reject should be (false)
  }


}

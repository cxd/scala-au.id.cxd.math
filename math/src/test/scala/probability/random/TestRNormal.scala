package probability.random

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

    // perform test for normality eg: Shapiro-Wilks test.

  }


}

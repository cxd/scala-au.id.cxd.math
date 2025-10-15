package probability.continuous

import au.id.cxd.math.probability.continuous.Exponential
import au.id.cxd.math.probability.random.RExp
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Exponential distribution
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Round-trip tests
  * - Memoryless property
  * - Moments
  */
class TestExponential extends FlatSpec with Matchers {

  val epsilon = 1e-8

  "Exponential distribution PDF at x=0" should "equal λ" in {
    val lambda = 2.0
    val exp = Exponential(lambda)
    val result = exp.pdf(0.0)

    Math.abs(result - lambda) should be < epsilon
  }

  "Exponential distribution PDF for negative x" should "equal 0" in {
    val exp = Exponential(2.0)
    exp.pdf(-1.0) should be (0.0)
  }

  "Exponential distribution PDF" should "decrease exponentially" in {
    val exp = Exponential(1.0)

    val pdf1 = exp.pdf(0.5)
    val pdf2 = exp.pdf(1.0)
    val pdf3 = exp.pdf(2.0)

    pdf1 should be > pdf2
    pdf2 should be > pdf3
  }

  "Exponential distribution CDF at x=0" should "equal 0" in {
    val exp = Exponential(2.0)
    exp.cdf(0.0) should be (0.0)
  }

  "Exponential distribution CDF for negative x" should "equal 0" in {
    val exp = Exponential(2.0)
    exp.cdf(-1.0) should be (0.0)
  }

  "Exponential distribution CDF" should "approach 1 for large x" in {
    val exp = Exponential(1.0)
    val result = exp.cdf(10.0)
    result should be > 0.9999
  }

  "Exponential distribution CDF" should "be monotonically increasing" in {
    val exp = Exponential(1.0)

    val cdf1 = exp.cdf(0.5)
    val cdf2 = exp.cdf(1.0)
    val cdf3 = exp.cdf(2.0)

    cdf1 should be < cdf2
    cdf2 should be < cdf3
  }

  "Exponential distribution CDF" should "equal 1-exp(-λx)" in {
    val lambda = 2.0
    val exp = Exponential(lambda)
    val x = 1.5

    val result = exp.cdf(x)
    val expected = 1.0 - Math.exp(-lambda * x)

    Math.abs(result - expected) should be < epsilon
  }

  "Exponential distribution InvCDF" should "satisfy round-trip property" in {
    val exp = Exponential(1.0)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = exp.invcdf(p)
      val pBack = exp.cdf(x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Exponential distribution CDF then InvCDF" should "return original x" in {
    val exp = Exponential(1.0)
    val testX = List(0.5, 1.0, 2.0, 4.0)

    testX.foreach { x =>
      val p = exp.cdf(x)
      val xBack = exp.invcdf(p)
      Math.abs(xBack - x) should be < epsilon
    }
  }

  "Exponential distribution InvCDF" should "equal -ln(1-p)/λ" in {
    val lambda = 2.0
    val exp = Exponential(lambda)
    val p = 0.5

    val result = exp.invcdf(p)
    val expected = -Math.log(1.0 - p) / lambda

    Math.abs(result - expected) should be < epsilon
  }

  "Exponential distribution mean" should "equal 1/λ" in {
    val lambda = 2.0
    val exp = Exponential(lambda)

    val result = exp.mean
    val expected = 1.0 / lambda

    Math.abs(result - expected) should be < epsilon
  }

  "Exponential distribution variance" should "equal 1/λ²" in {
    val lambda = 2.0
    val exp = Exponential(lambda)

    val result = exp.variance
    val expected = 1.0 / (lambda * lambda)

    Math.abs(result - expected) should be < epsilon
  }

  "Exponential distribution" should "satisfy memoryless property" in {
    val exp = Exponential(1.0)
    val s = 1.0
    val t = 2.0

    // P(X > s+t | X > s) = P(X > t)
    val pGtS = 1.0 - exp.cdf(s)
    val pGtSPlusT = 1.0 - exp.cdf(s + t)
    val pGtT = 1.0 - exp.cdf(t)

    val conditional = pGtSPlusT / pGtS
    val unconditional = pGtT

    Math.abs(conditional - unconditional) should be < epsilon
  }

  "Exponential distribution median" should "equal ln(2)/λ" in {
    val lambda = 2.0
    val exp = Exponential(lambda)

    val result = exp.invcdf(0.5)
    val expected = Math.log(2.0) / lambda

    Math.abs(result - expected) should be < epsilon
  }

  "Exponential distribution sample" should "generate positive values" in {
    val exp = RExp(1.0)
    val samples = for (_ <- 1 to 100) yield exp.draw()

    samples.foreach { x =>
      x should be > 0.0
    }
  }

  "Exponential distribution sample mean" should "approximate 1/λ" in {
    val lambda = 1.0
    val exp = RExp(lambda)
    val samples = for (_ <- 1 to 10000) yield exp.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean - exp.mean) should be < 0.05
  }

  "Exponential distribution integral" should "equal CDF difference" in {
    val exp = Exponential(1.0)

    val integral = exp.integral(0.5, 3.0)
    val expected = exp.cdf(3.0) - exp.cdf(0.5)

    Math.abs(integral - expected) should be < epsilon
  }

  "Exponential distribution quartiles" should "be properly ordered" in {
    val exp = Exponential(1.0)

    val q25 = exp.invcdf(0.25)
    val q50 = exp.invcdf(0.5)
    val q75 = exp.invcdf(0.75)

    q25 should be < q50
    q50 should be < q75

    q25 should be > 0.0
  }

  "Exponential distribution with different rates" should "have inverse relationship with mean" in {
    val exp1 = Exponential(1.0)
    val exp2 = Exponential(2.0)

    exp1.mean should be > exp2.mean
    Math.abs(exp1.mean - 2.0 * exp2.mean) should be < epsilon
  }
}

package probability.continuous

import au.id.cxd.math.probability.continuous.StudentT
import au.id.cxd.math.probability.random.RStudentT
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Student's t-distribution
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Round-trip tests
  * - Known critical values
  * - Symmetry properties
  */
class TestStudentT extends FlatSpec with Matchers {

  val epsilon = 1e-4

  "Student's t distribution PDF at x=0" should "be maximum for symmetric df" in {
    val t = StudentT(10.0)
    val pdf0 = t.pdf(0.0)
    val pdfLeft = t.pdf(-0.5)
    val pdfRight = t.pdf(0.5)

    pdf0 should be > pdfLeft
    pdf0 should be > pdfRight
  }

  "Student's t distribution PDF" should "be symmetric around mean" in {
    val t = StudentT(10.0)

    val pdfLeft = t.pdf(-2.0)
    val pdfRight = t.pdf(2.0)

    Math.abs(pdfLeft - pdfRight) should be < epsilon
  }

  "Student's t distribution CDF at x=0" should "equal 0.5" in {
    val t = StudentT(10.0)
    val result = t.cdf(0.0)

    Math.abs(result - 0.5) should be < epsilon
  }

  "Student's t distribution CDF" should "satisfy symmetry: CDF(-x) = 1 - CDF(x)" in {
    val t = StudentT(10.0)
    val testX = List(0.5, 1.0, 2.0)

    testX.foreach { x =>
      val cdfLeft = t.cdf(-x)
      val cdfRight = t.cdf(x)
      Math.abs(cdfLeft + cdfRight - 1.0) should be < epsilon
    }
  }

  "Student's t distribution CDF" should "be monotonically increasing" in {
    val t = StudentT(10.0)

    val cdf1 = t.cdf(-2.0)
    val cdf2 = t.cdf(0.0)
    val cdf3 = t.cdf(2.0)

    cdf1 should be < cdf2
    cdf2 should be < cdf3
  }

  "Student's t distribution InvCDF at p=0.5" should "equal 0" in {
    val t = StudentT(10.0)
    val result = t.invcdf(0.5)

    Math.abs(result) should be < epsilon
  }

  "Student's t distribution InvCDF" should "satisfy symmetry: InvCDF(1-p) = -InvCDF(p)" in {
    val t = StudentT(10.0)
    val testP = List(0.1, 0.25, 0.4)

    testP.foreach { p =>
      val qLow = t.invcdf(p)
      val qHigh = t.invcdf(1.0 - p)
      Math.abs(qLow + qHigh) should be < epsilon
    }
  }

  "Student's t distribution InvCDF" should "satisfy round-trip property" in {
    val t = StudentT(10.0)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = t.invcdf(p)
      val pBack = t.cdf(x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Student's t distribution CDF then InvCDF" should "return original x" in {
    val t = StudentT(10.0)
    val testX = List(-2.0, -1.0, 0.0, 1.0, 2.0)

    testX.foreach { x =>
      val p = t.cdf(x)
      val xBack = t.invcdf(p)
      Math.abs(xBack - x) should be < epsilon
    }
  }

  "Student's t distribution with df=1" should "have heavy tails (Cauchy)" in {
    val t = StudentT(1.0)

    // For df=1 (Cauchy), CDF at x=1 should be 0.75
    val result = t.cdf(1.0)
    val expected = 0.75

    Math.abs(result - expected) should be < 0.02
  }

  "Student's t distribution with large df" should "approximate standard normal" in {
    val t = StudentT(100.0)

    // For large df, should approximate N(0,1)
    // N(0,1) CDF at x=1.96 is approximately 0.975
    val result = t.cdf(1.96)
    val expected = 0.975

    Math.abs(result - expected) should be < 0.01
  }

  "Student's t critical value for df=10, p=0.975" should "be approximately 2.228" in {
    val t = StudentT(10.0)
    val result = t.invcdf(0.975)
    val expected = 2.228

    Math.abs(result - expected) should be < 0.05
  }

  "Student's t critical value for df=10, p=0.95" should "be approximately 1.812" in {
    val t = StudentT(10.0)
    val result = t.invcdf(0.95)
    val expected = 1.812

    Math.abs(result - expected) should be < 0.05
  }

  "Student's t critical value for df=5, p=0.975" should "be approximately 2.571" in {
    val t = StudentT(5.0)
    val result = t.invcdf(0.975)
    val expected = 2.571

    Math.abs(result - expected) should be < 0.05
  }

  "Student's t distribution mean" should "equal 0 for standard t" in {
    val t = StudentT(10.0)
    val result = t.mean

    Math.abs(result) should be < epsilon
  }

  "Student's t distribution with location parameter" should "shift mean" in {
    val mu = 5.0
    val t = StudentT(10.0, mu, 1.0)
    val result = t.mean

    Math.abs(result - mu) should be < epsilon
  }

  "Student's t distribution variance for df>2" should "equal df/(df-2) * σ²" in {
    val df = 10.0
    val sigma = 1.0
    val t = StudentT(df, 0.0, sigma)

    val result = t.variance
    val expected = df / (df - 2.0) * sigma * sigma

    Math.abs(result - expected) should be < epsilon
  }

  "Student's t distribution sample" should "generate values around mean" in {
    val t = RStudentT(10.0)
    val samples = for (_ <- 1 to 100) yield t.draw()

    // Most samples should be within reasonable range
    val inRange = samples.count(x => Math.abs(x) < 5.0)
    inRange should be > 80
  }

  "Student's t distribution sample mean" should "approximate 0" in {
    val t = RStudentT(10.0)
    val samples = for (_ <- 1 to 10000) yield t.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean) should be < 0.05
  }

  "Student's t distribution integral" should "equal CDF difference" in {
    val t = StudentT(10.0)

    val integral = t.integral(-1.0, 1.0)
    val expected = t.cdf(1.0) - t.cdf(-1.0)

    Math.abs(integral - expected) should be < epsilon
  }
}

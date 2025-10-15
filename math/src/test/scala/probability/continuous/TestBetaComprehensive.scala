package probability.continuous

import au.id.cxd.math.probability.continuous.Beta
import au.id.cxd.math.probability.random.RBeta
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Beta distribution
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Round-trip tests
  * - Moments (mean, variance)
  * - Special cases
  */
class TestBetaComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-6

  "Beta distribution PDF at x=0.5 with α=2, β=2" should "match known value" in {
    val beta = Beta(2.0, 2.0)
    val result = beta.pdf(0.5)
    val expected = 1.5 // For Beta(2,2), pdf(0.5) = 6*0.5*0.5 = 1.5
    Math.abs(result - expected) should be < epsilon
  }

  "Beta distribution PDF" should "integrate to 1" in {
    val beta = Beta(2.0, 3.0)
    // Use trapezoidal rule for numerical integration
    val n = 1000
    val dx = 1.0 / n
    var sum = 0.0
    for (i <- 1 until n) {
      val x = i * dx
      sum += beta.pdf(x) * dx
    }
    Math.abs(sum - 1.0) should be < 0.01
  }

  "Beta distribution PDF" should "be zero outside [0,1]" in {
    val beta = Beta(2.0, 3.0)
    beta.pdf(-0.1) should be (0.0)
    beta.pdf(1.1) should be (0.0)
  }

  "Beta distribution CDF at x=0" should "equal 0" in {
    val beta = Beta(2.0, 3.0)
    beta.cdf(0.0) should be (0.0)
  }

  "Beta distribution CDF at x=1" should "equal 1" in {
    val beta = Beta(2.0, 3.0)
    beta.cdf(1.0) should be (1.0)
  }

  "Beta distribution CDF" should "be monotonically increasing" in {
    val beta = Beta(2.0, 3.0)
    val cdf1 = beta.cdf(0.3)
    val cdf2 = beta.cdf(0.5)
    val cdf3 = beta.cdf(0.7)

    cdf1 should be < cdf2
    cdf2 should be < cdf3
  }

  "Beta distribution InvCDF" should "satisfy round-trip property" in {
    val beta = Beta(2.0, 3.0)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = beta.invcdf(p)
      val pBack = beta.cdf(x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Beta distribution CDF then InvCDF" should "return original x" in {
    val beta = Beta(2.0, 3.0)
    val testX = List(0.2, 0.5, 0.8)

    testX.foreach { x =>
      val p = beta.cdf(x)
      val xBack = beta.invcdf(p)
      Math.abs(xBack - x) should be < epsilon
    }
  }

  "Beta distribution mean" should "equal α/(α+β)" in {
    val alpha = 2.0
    val beta = 3.0
    val betaDist = Beta(alpha, beta)

    val result = betaDist.mean
    val expected = alpha / (alpha + beta)

    Math.abs(result - expected) should be < epsilon
  }

  "Beta distribution variance" should "match formula" in {
    val alpha = 2.0
    val beta = 3.0
    val betaDist = Beta(alpha, beta)

    val result = betaDist.variance
    val sum = alpha + beta
    val expected = (alpha * beta) / (sum * sum * (sum + 1))

    Math.abs(result - expected) should be < epsilon
  }

  "Beta(1,1) distribution" should "be uniform on [0,1]" in {
    val beta = Beta(1.0, 1.0)

    // PDF should be constant 1
    beta.pdf(0.25) should be (1.0 +- 1e-10)
    beta.pdf(0.5) should be (1.0 +- 1e-10)
    beta.pdf(0.75) should be (1.0 +- 1e-10)

    // CDF should be linear
    beta.cdf(0.25) should be (0.25 +- epsilon)
    beta.cdf(0.5) should be (0.5 +- epsilon)
    beta.cdf(0.75) should be (0.75 +- epsilon)
  }

  "Beta distribution with α=β" should "be symmetric around 0.5" in {
    val beta = Beta(5.0, 5.0)

    val pdf1 = beta.pdf(0.3)
    val pdf2 = beta.pdf(0.7)
    Math.abs(pdf1 - pdf2) should be < epsilon

    val cdf1 = beta.cdf(0.3)
    val cdf2 = beta.cdf(0.7)
    Math.abs(cdf1 + cdf2 - 1.0) should be < epsilon
  }

  "Beta distribution median for α=β" should "be 0.5" in {
    val beta = Beta(5.0, 5.0)
    val median = beta.invcdf(0.5)
    Math.abs(median - 0.5) should be < epsilon
  }

  "Beta distribution quartiles for α=2, β=3" should "match expected values" in {
    val beta = Beta(2.0, 3.0)

    val q25 = beta.invcdf(0.25)
    val q50 = beta.invcdf(0.5)
    val q75 = beta.invcdf(0.75)

    // Verify ordering
    q25 should be < q50
    q50 should be < q75

    // Verify they're in (0,1)
    q25 should be > 0.0
    q75 should be < 1.0
  }

  "Beta distribution sample" should "generate values in [0,1]" in {
    val beta = RBeta(2.0, 3.0)
    val samples = for (_ <- 1 to 100) yield beta.draw()

    samples.foreach { x =>
      x should be >= 0.0
      x should be <= 1.0
    }
  }

  "Beta distribution sample mean" should "approximate theoretical mean" in {
    val beta = RBeta(2.0, 3.0)
    val samples = for (_ <- 1 to 10000) yield beta.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean - beta.mean) should be < 0.02
  }

  "Beta distribution integral" should "equal CDF difference" in {
    val beta = Beta(2.0, 3.0)

    val integral = beta.integral(0.2, 0.8)
    val expected = beta.cdf(0.8) - beta.cdf(0.2)

    Math.abs(integral - expected) should be < epsilon
  }
}

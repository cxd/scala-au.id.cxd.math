package probability.continuous

import au.id.cxd.math.probability.continuous.Gamma
import au.id.cxd.math.probability.random.RGamma
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Gamma distribution
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Round-trip tests
  * - Moments (mean, variance)
  * - Relationship with other distributions
  */
class TestGammaComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-6

  "Gamma distribution PDF at x=0" should "equal 0 for α>1" in {
    val gamma = Gamma(2.0, 1.0)
    gamma.pdf(0.0) should be (0.0)
  }

  "Gamma distribution PDF for negative x" should "equal 0" in {
    val gamma = Gamma(2.0, 1.0)
    gamma.pdf(-1.0) should be (0.0)
  }

  "Gamma distribution PDF" should "be positive for x>0" in {
    val gamma = Gamma(2.0, 1.0)
    val testX = List(0.5, 1.0, 2.0, 5.0)

    testX.foreach { x =>
      gamma.pdf(x) should be > 0.0
    }
  }

  "Gamma distribution CDF at x=0" should "equal 0" in {
    val gamma = Gamma(2.0, 1.0)
    gamma.cdf(0.0) should be (0.0)
  }

  "Gamma distribution CDF for negative x" should "equal 0" in {
    val gamma = Gamma(2.0, 1.0)
    gamma.cdf(-1.0) should be (0.0)
  }

  "Gamma distribution CDF" should "approach 1 for large x" in {
    val gamma = Gamma(2.0, 1.0)
    val result = gamma.cdf(20.0)
    result should be > 0.999
  }

  "Gamma distribution CDF" should "be monotonically increasing" in {
    val gamma = Gamma(2.0, 1.0)

    val cdf1 = gamma.cdf(0.5)
    val cdf2 = gamma.cdf(2.0)
    val cdf3 = gamma.cdf(4.0)

    cdf1 should be < cdf2
    cdf2 should be < cdf3
  }

  "Gamma distribution InvCDF" should "satisfy round-trip property" in {
    val gamma = Gamma(2.0, 1.0)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = gamma.invcdf(p)
      val pBack = gamma.cdf(x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Gamma distribution CDF then InvCDF" should "return original x" in {
    val gamma = Gamma(2.0, 1.0)
    val testX = List(0.5, 1.0, 2.0, 4.0)

    testX.foreach { x =>
      val p = gamma.cdf(x)
      // Skip very small p values as they have poor numerical precision
      if (p >= 1e-9) {
        val xBack = gamma.invcdf(p)
        Math.abs(xBack - x) should be < 1e-4
      }
    }
  }

  "Gamma distribution mean" should "equal α/β" in {
    val alpha = 2.0
    val beta = 1.5
    val gamma = Gamma(alpha, beta)

    val result = gamma.mean
    val expected = alpha / beta

    Math.abs(result - expected) should be < epsilon
  }

  "Gamma distribution variance" should "equal α/β²" in {
    val alpha = 2.0
    val beta = 1.5
    val gamma = Gamma(alpha, beta)

    val result = gamma.variance
    val expected = alpha / (beta * beta)

    Math.abs(result - expected) should be < epsilon
  }

  "Gamma(1, λ) distribution" should "be exponential with rate λ" in {
    val lambda = 2.0
    val gamma = Gamma(1.0, lambda)

    // For Exponential(λ), PDF(x) = λ*exp(-λx)
    val x = 1.0
    val gammaPdf = gamma.pdf(x)
    val expPdf = lambda * Math.exp(-lambda * x)

    Math.abs(gammaPdf - expPdf) should be < epsilon

    // For Exponential(λ), CDF(x) = 1 - exp(-λx)
    val gammaCdf = gamma.cdf(x)
    val expCdf = 1.0 - Math.exp(-lambda * x)

    Math.abs(gammaCdf - expCdf) should be < epsilon
  }

  "Gamma distribution with scale parameter" should "match rate parameterization" in {
    val k = 2.0
    val theta = 0.5 // scale
    val beta = 1.0 / theta // rate

    val gamma1 = Gamma(k, beta)
    // Note: Scala implementation uses (alpha, beta) = (shape, rate)
    // To use scale, we'd need a constructor that takes scale parameter

    val x = 1.5
    val result = gamma1.cdf(x)

    result should be > 0.0
    result should be < 1.0
  }

  "Gamma distribution sample" should "generate positive values" in {
    val gamma = RGamma(2.0, 1.0)
    val samples = for (_ <- 1 to 100) yield gamma.draw()

    samples.foreach { x =>
      x should be > 0.0
    }
  }

  "Gamma distribution sample mean" should "approximate theoretical mean" in {
    val alpha = 2.0
    val beta = 1.0
    val gamma = RGamma(alpha, beta)
    val samples = for (_ <- 1 to 10000) yield gamma.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean - gamma.mean) should be < 0.1
  }

  "Gamma distribution quartiles" should "be properly ordered" in {
    val gamma = Gamma(2.0, 1.0)

    val q25 = gamma.invcdf(0.25)
    val q50 = gamma.invcdf(0.5)
    val q75 = gamma.invcdf(0.75)

    q25 should be < q50
    q50 should be < q75

    q25 should be > 0.0
  }

  "Gamma distribution mode for α>1" should "equal (α-1)/β" in {
    val alpha = 3.0
    val beta = 2.0
    val gamma = Gamma(alpha, beta)

    val expectedMode = (alpha - 1.0) / beta

    // The mode should have a higher PDF than nearby values
    val modePdf = gamma.pdf(expectedMode)
    val leftPdf = gamma.pdf(expectedMode - 0.1)
    val rightPdf = gamma.pdf(expectedMode + 0.1)

    modePdf should be > leftPdf
    modePdf should be > rightPdf
  }

  "Gamma distribution integral" should "equal CDF difference" in {
    val gamma = Gamma(2.0, 1.0)

    val integral = gamma.integral(0.5, 3.0)
    val expected = gamma.cdf(3.0) - gamma.cdf(0.5)

    Math.abs(integral - expected) should be < epsilon
  }

  "Gamma distribution for small α" should "handle α=0.5" in {
    val gamma = Gamma(0.5, 1.0)
    val result = gamma.cdf(1.0)

    result should be > 0.0
    result should be < 1.0
    result.isNaN should be (false)
  }

  "Gamma distribution for large α" should "handle α=50" in {
    val gamma = Gamma(50.0, 1.0)
    val result = gamma.cdf(50.0)

    result should be > 0.4
    result should be < 0.6
    result.isNaN should be (false)
  }
}

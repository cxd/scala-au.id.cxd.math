package probability.continuous

import au.id.cxd.math.probability.continuous.ChiSquare
import au.id.cxd.math.probability.random.RChisq
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Chi-Square distribution
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Known critical values
  * - Relationship with Gamma distribution
  */
class TestChiSquareComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-4

  "Chi-square distribution PDF for x<0" should "equal 0" in {
    val chi = ChiSquare(5)
    chi.pdf(-1.0) should be (0.0)
  }

  "Chi-square distribution PDF at x=0" should "depend on df" in {
    val chi1 = ChiSquare(1)
    val chi2 = ChiSquare(2)
    val chi3 = ChiSquare(3)

    // For df=2, pdf(0) should be non-zero
    chi2.pdf(0.0) should be > 0.0

    // For df>2, pdf(0) should be 0
    chi3.pdf(0.0) should be (0.0)
  }

  "Chi-square distribution PDF" should "be positive for x>0" in {
    val chi = ChiSquare(5)
    val testX = List(0.5, 2.0, 5.0, 10.0)

    testX.foreach { x =>
      chi.pdf(x) should be > 0.0
    }
  }

  "Chi-square distribution CDF at x=0" should "equal 0" in {
    val chi = ChiSquare(5)
    chi.cdf(0.0) should be (0.0 +- epsilon)
  }

  "Chi-square distribution CDF for x<0" should "equal 0" in {
    val chi = ChiSquare(5)
    chi.cdf(-1.0) should be (0.0)
  }

  "Chi-square distribution CDF" should "approach 1 for large x" in {
    val chi = ChiSquare(5)
    val result = chi.cdf(30.0)
    result should be > 0.999
  }

  "Chi-square distribution CDF" should "be monotonically increasing" in {
    val chi = ChiSquare(5)

    val cdf1 = chi.cdf(1.0)
    val cdf2 = chi.cdf(5.0)
    val cdf3 = chi.cdf(10.0)

    cdf1 should be < cdf2
    cdf2 should be < cdf3
  }

  "Chi-square distribution InvCDF" should "satisfy round-trip property" in {
    val chi = ChiSquare(5)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = chi.invcdf(p)
      val pBack = chi.cdf(x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Chi-square distribution CDF then InvCDF" should "return original x" in {
    val chi = ChiSquare(5)
    val testX = List(1.0, 3.0, 5.0, 10.0)

    testX.foreach { x =>
      val p = chi.cdf(x)
      val xBack = chi.invcdf(p)
      Math.abs(xBack - x) should be < 1e-4
    }
  }

  "Chi-square distribution with df=1" should "have known CDF values" in {
    val chi = ChiSquare(1)

    // 95th percentile for df=1 is approximately 3.841
    val result = chi.invcdf(0.95)
    val expected = 3.841

    Math.abs(result - expected) should be < 0.01
  }

  "Chi-square distribution with df=2" should "have known CDF values" in {
    val chi = ChiSquare(2)

    // 95th percentile for df=2 is approximately 5.991
    val result = chi.invcdf(0.95)
    val expected = 5.991

    Math.abs(result - expected) should be < 0.01
  }

  "Chi-square distribution with df=5" should "have known CDF values" in {
    val chi = ChiSquare(5)

    // 95th percentile for df=5 is approximately 11.070
    val result = chi.invcdf(0.95)
    val expected = 11.070

    Math.abs(result - expected) should be < 0.05
  }

  "Chi-square distribution with df=1" should "have known 99th percentile" in {
    val chi = ChiSquare(1)

    // 99th percentile for df=1 is approximately 6.635
    val result = chi.invcdf(0.99)
    val expected = 6.635

    Math.abs(result - expected) should be < 0.05
  }

  "Chi-square distribution mean" should "equal df" in {
    val k = 5.0
    val chi = ChiSquare(k.toInt)

    val result = chi.mean
    Math.abs(result - k) should be < epsilon
  }

  "Chi-square distribution variance" should "equal 2*df" in {
    val k = 5.0
    val chi = ChiSquare(k.toInt)

    val result = chi.variance
    val expected = 2.0 * k

    Math.abs(result - expected) should be < epsilon
  }

  "Chi-square distribution median for df=1" should "be approximately 0.455" in {
    val chi = ChiSquare(1)
    val result = chi.invcdf(0.5)
    val expected = 0.455

    Math.abs(result - expected) should be < 0.05
  }

  "Chi-square distribution median for df=2" should "be approximately 1.386" in {
    val chi = ChiSquare(2)
    val result = chi.invcdf(0.5)
    val expected = 1.386

    Math.abs(result - expected) should be < 0.05
  }

  "Chi-square distribution is Gamma" should "with shape=k/2 and rate=1/2" in {
    val k = 6
    val chi = ChiSquare(k)

    // ChiSquare(k) = Gamma(k/2, 1/2)
    // Mean: k/2 / (1/2) = k
    Math.abs(chi.mean - k.toDouble) should be < epsilon

    // Variance: (k/2) / (1/2)^2 = 2k
    Math.abs(chi.variance - 2.0 * k) should be < epsilon
  }

  "Chi-square distribution sample" should "generate positive values" in {
    val chi = RChisq(5)
    val samples = for (_ <- 1 to 100) yield chi.draw()

    samples.foreach { x =>
      x should be > 0.0
    }
  }

  "Chi-square distribution sample mean" should "approximate df" in {
    val k = 5
    val chi = RChisq(k)
    val samples = for (_ <- 1 to 10000) yield chi.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean - k.toDouble) should be < 0.1
  }

  "Chi-square distribution for small df" should "handle df=1" in {
    val chi = ChiSquare(1)
    val result = chi.cdf(1.0)

    result should be > 0.0
    result should be < 1.0
    result.isNaN should be (false)
  }

  "Chi-square distribution for large df" should "approach normal" in {
    val chi = ChiSquare(100)
    // For large df, ChiSquare approaches N(df, 2*df)
    // Median should be close to df
    val median = chi.invcdf(0.5)

    Math.abs(median - 100.0) should be < 2.0
  }

  "Chi-square distribution integral" should "equal CDF difference" in {
    val chi = ChiSquare(5)

    val integral = chi.integral(2.0, 8.0)
    val expected = chi.cdf(8.0) - chi.cdf(2.0)

    Math.abs(integral - expected) should be < epsilon
  }
}

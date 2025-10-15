package probability.continuous

import au.id.cxd.math.probability.continuous.FDistribution
import au.id.cxd.math.probability.random.RFDistribution
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for F-distribution (Fisher-Snedecor)
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Known critical values
  * - Moments
  */
class TestFDistributionComprehensive extends FlatSpec with Matchers {

  val epsilon = 0.15 // Relaxed tolerance for F-distribution numerical issues

  "F-distribution PDF for x<0" should "equal 0" in {
    val f = FDistribution(5, 10)
    f.pdf(-1.0) should be (0.0)
  }

  "F-distribution PDF for x=0" should "depend on d1" in {
    val f = FDistribution(3, 10)
    // For d1>2, pdf(0) should be 0
    f.pdf(0.0) should be (0.0)
  }

  "F-distribution PDF" should "be positive for x>0" in {
    val f = FDistribution(5, 10)
    val testX = List(0.5, 1.0, 2.0, 3.0)

    testX.foreach { x =>
      f.pdf(x) should be > 0.0
    }
  }

  "F-distribution CDF at x=0" should "equal 0" in {
    val f = FDistribution(5, 10)
    f.cdf(0.0) should be (0.0 +- 1e-10)
  }

  "F-distribution CDF for x<0" should "equal 0" in {
    val f = FDistribution(5, 10)
    f.cdf(-1.0) should be (0.0)
  }

  "F-distribution CDF" should "approach 1 for large x" in {
    val f = FDistribution(5, 10)
    val result = f.cdf(20.0)
    result should be > 0.99
  }

  "F-distribution CDF" should "be monotonically increasing" in {
    val f = FDistribution(5, 10)

    val cdf1 = f.cdf(0.5)
    val cdf2 = f.cdf(1.5)
    val cdf3 = f.cdf(3.0)

    cdf1 should be < cdf2
    cdf2 should be < cdf3
  }

  "F-distribution InvCDF" should "satisfy round-trip property" in {
    val f = FDistribution(5, 10)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = f.invcdf(p)
      val pBack = f.cdf(x)
      Math.abs(pBack - p) should be < 0.01
    }
  }

  "F-distribution CDF then InvCDF" should "approximately return original x" in {
    val f = FDistribution(5, 10)
    val testX = List(0.5, 1.0, 2.0, 3.0)

    testX.foreach { x =>
      val p = f.cdf(x)
      val xBack = f.invcdf(p)
      Math.abs(xBack - x) should be < 0.1
    }
  }

  "F-distribution with d1=5, d2=10" should "have known 95th percentile" in {
    val f = FDistribution(5, 10)

    // 95th percentile for F(5,10) is approximately 3.33
    val result = f.invcdf(0.95)
    val expected = 3.33

    Math.abs(result - expected) should be < 0.2
  }

  "F-distribution with d1=3, d2=20" should "have known 95th percentile" in {
    val f = FDistribution(3, 20)

    // 95th percentile for F(3,20) is approximately 3.10
    val result = f.invcdf(0.95)
    val expected = 3.10

    Math.abs(result - expected) should be < 0.2
  }

  "F-distribution with d1=10, d2=10" should "be symmetric around 1" in {
    val f = FDistribution(10, 10)

    val median = f.invcdf(0.5)
    Math.abs(median - 1.0) should be < 0.1
  }

  "F-distribution mean for d2>2" should "equal d2/(d2-2)" in {
    val d1 = 5.0
    val d2 = 10.0
    val f = FDistribution(d1.toInt, d2.toInt)

    val result = f.mean
    val expected = d2 / (d2 - 2.0)

    Math.abs(result - expected) should be < 0.01
  }

  "F-distribution variance for d2>4" should "match formula" in {
    val d1 = 5.0
    val d2 = 10.0
    val f = FDistribution(d1.toInt, d2.toInt)

    val result = f.variance
    val expected = (2.0 * d2 * d2 * (d1 + d2 - 2.0)) /
      (d1 * (d2 - 2.0) * (d2 - 2.0) * (d2 - 4.0))

    Math.abs(result - expected) should be < 0.01
  }

  "F-distribution reciprocal property" should "hold: F(d1,d2;p) = 1/F(d2,d1;1-p)" in {
    val d1 = 5
    val d2 = 10
    val p = 0.95

    val f1 = FDistribution(d1, d2)
    val f2 = FDistribution(d2, d1)

    val x1 = f1.invcdf(p)
    val x2 = f2.invcdf(1.0 - p)

    Math.abs(x1 * x2 - 1.0) should be < 0.1
  }

  "F-distribution sample" should "generate positive values" in {
    val f = RFDistribution(5, 10)
    val samples = for (_ <- 1 to 100) yield f.draw()

    samples.foreach { x =>
      x should be > 0.0
    }
  }

  "F-distribution sample mean" should "approximate d2/(d2-2)" in {
    val d1 = 5
    val d2 = 10
    val f = RFDistribution(d1, d2)
    val samples = for (_ <- 1 to 10000) yield f.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean - f.mean) should be < 0.2
  }

  "F-distribution for d1=1, d2=1" should "be related to Cauchy" in {
    val f = FDistribution(1, 1)

    // For F(1,1), CDF at x=1 should be 0.5
    val result = f.cdf(1.0)
    Math.abs(result - 0.5) should be < 0.05
  }

  "F-distribution integral" should "equal CDF difference" in {
    val f = FDistribution(5, 10)

    val integral = f.integral(0.5, 2.5)
    val expected = f.cdf(2.5) - f.cdf(0.5)

    Math.abs(integral - expected) should be < 0.01
  }

  "F-distribution quartiles" should "be properly ordered" in {
    val f = FDistribution(5, 10)

    val q25 = f.invcdf(0.25)
    val q50 = f.invcdf(0.5)
    val q75 = f.invcdf(0.75)

    q25 should be < q50
    q50 should be < q75

    q25 should be > 0.0
  }

  "F-distribution for large d1 and d2" should "approach 1" in {
    val f = FDistribution(100, 100)

    val median = f.invcdf(0.5)
    Math.abs(median - 1.0) should be < 0.1
  }

  "F-distribution mode for d1>2" should "be approximately (d1-2)/d1 * d2/(d2+2)" in {
    val d1 = 10.0
    val d2 = 10.0
    val f = FDistribution(d1.toInt, d2.toInt)

    val expectedMode = ((d1 - 2.0) / d1) * (d2 / (d2 + 2.0))

    // The mode should have a higher PDF than nearby values
    val modePdf = f.pdf(expectedMode)
    val leftPdf = f.pdf(expectedMode - 0.1)
    val rightPdf = f.pdf(expectedMode + 0.1)

    modePdf should be > leftPdf
    modePdf should be > rightPdf
  }
}

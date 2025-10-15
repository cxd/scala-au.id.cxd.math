package probability.continuous

import au.id.cxd.math.probability.continuous.Uniform
import au.id.cxd.math.probability.random.RUniform
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Uniform distribution U(a,b)
  *
  * Tests include:
  * - PDF properties
  * - CDF properties
  * - InvCDF (quantile function)
  * - Round-trip tests
  * - Moments
  */
class TestUniformComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-10

  "Uniform distribution PDF inside [a,b]" should "be constant 1/(b-a)" in {
    val uniform = Uniform(0.0, 1.0)

    uniform.pdf(0.25) should be (1.0 +- epsilon)
    uniform.pdf(0.5) should be (1.0 +- epsilon)
    uniform.pdf(0.75) should be (1.0 +- epsilon)
  }

  "Uniform distribution PDF outside [a,b]" should "equal 0" in {
    val uniform = Uniform(0.0, 1.0)

    uniform.pdf(-0.5) should be (0.0)
    uniform.pdf(1.5) should be (0.0)
  }

  "Uniform distribution PDF" should "scale inversely with interval width" in {
    val uniform1 = Uniform(0.0, 1.0)
    val uniform2 = Uniform(0.0, 2.0)

    val pdf1 = uniform1.pdf(0.5)
    val pdf2 = uniform2.pdf(0.5)

    Math.abs(pdf1 - 2.0 * pdf2) should be < epsilon
  }

  "Uniform distribution CDF at lower bound" should "equal 0" in {
    val uniform = Uniform(2.0, 5.0)
    uniform.cdf(2.0) should be (0.0 +- epsilon)
  }

  "Uniform distribution CDF at upper bound" should "equal 1" in {
    val uniform = Uniform(2.0, 5.0)
    uniform.cdf(5.0) should be (1.0 +- epsilon)
  }

  "Uniform distribution CDF below lower bound" should "equal 0" in {
    val uniform = Uniform(2.0, 5.0)
    uniform.cdf(1.0) should be (0.0)
  }

  "Uniform distribution CDF above upper bound" should "equal 1" in {
    val uniform = Uniform(2.0, 5.0)
    uniform.cdf(6.0) should be (1.0)
  }

  "Uniform distribution CDF inside [a,b]" should "be linear" in {
    val a = 0.0
    val b = 1.0
    val uniform = Uniform(a, b)

    val testX = List(0.25, 0.5, 0.75)

    testX.foreach { x =>
      val result = uniform.cdf(x)
      val expected = (x - a) / (b - a)
      Math.abs(result - expected) should be < epsilon
    }
  }

  "Uniform distribution InvCDF" should "be linear transformation" in {
    val a = 2.0
    val b = 5.0
    val uniform = Uniform(a, b)

    val testP = List(0.0, 0.25, 0.5, 0.75, 1.0)

    testP.foreach { p =>
      val result = uniform.invcdf(p)
      val expected = a + p * (b - a)
      Math.abs(result - expected) should be < epsilon
    }
  }

  "Uniform distribution InvCDF" should "satisfy round-trip property" in {
    val uniform = Uniform(0.0, 1.0)
    val testP = List(0.1, 0.25, 0.5, 0.75, 0.9)

    testP.foreach { p =>
      val x = uniform.invcdf(p)
      val pBack = uniform.cdf(x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Uniform distribution CDF then InvCDF" should "return original x" in {
    val uniform = Uniform(0.0, 1.0)
    val testX = List(0.2, 0.5, 0.8)

    testX.foreach { x =>
      val p = uniform.cdf(x)
      val xBack = uniform.invcdf(p)
      Math.abs(xBack - x) should be < epsilon
    }
  }

  "Uniform distribution mean" should "equal (a+b)/2" in {
    val a = 2.0
    val b = 5.0
    val uniform = Uniform(a, b)

    val result = uniform.mean
    val expected = (a + b) / 2.0

    Math.abs(result - expected) should be < epsilon
  }

  "Uniform distribution variance" should "equal (b-a)²/12" in {
    val a = 2.0
    val b = 5.0
    val uniform = Uniform(a, b)

    val result = uniform.variance
    val expected = Math.pow(b - a, 2) / 12.0

    Math.abs(result - expected) should be < epsilon
  }

  "Uniform(0,1) distribution median" should "equal 0.5" in {
    val uniform = Uniform(0.0, 1.0)
    val result = uniform.invcdf(0.5)

    Math.abs(result - 0.5) should be < epsilon
  }

  "Uniform distribution quartiles" should "divide [a,b] into equal parts" in {
    val a = 0.0
    val b = 4.0
    val uniform = Uniform(a, b)

    val q25 = uniform.invcdf(0.25)
    val q50 = uniform.invcdf(0.5)
    val q75 = uniform.invcdf(0.75)

    Math.abs(q25 - 1.0) should be < epsilon
    Math.abs(q50 - 2.0) should be < epsilon
    Math.abs(q75 - 3.0) should be < epsilon
  }

  "Uniform distribution sample" should "generate values in [a,b]" in {
    val a = 2.0
    val b = 5.0
    val uniform = RUniform(a, b)
    val samples = for (_ <- 1 to 100) yield uniform.draw()

    samples.foreach { x =>
      x should be >= a
      x should be <= b
    }
  }

  "Uniform distribution sample mean" should "approximate (a+b)/2" in {
    val a = 0.0
    val b = 1.0
    val uniform = RUniform(a, b)
    val samples = for (_ <- 1 to 10000) yield uniform.draw()
    val sampleMean = samples.sum / samples.length

    Math.abs(sampleMean - uniform.mean) should be < 0.01
  }

  "Uniform distribution integral" should "equal CDF difference" in {
    val uniform = Uniform(0.0, 1.0)

    val integral = uniform.integral(0.2, 0.8)
    val expected = uniform.cdf(0.8) - uniform.cdf(0.2)

    Math.abs(integral - expected) should be < epsilon
  }

  "Uniform distribution for general [a,b]" should "have all properties" in {
    val a = -3.0
    val b = 7.0
    val uniform = Uniform(a, b)

    // PDF constant in range
    val midpoint = (a + b) / 2.0
    val pdf = uniform.pdf(midpoint)
    Math.abs(pdf - 1.0 / (b - a)) should be < epsilon

    // CDF at midpoint is 0.5
    val cdf = uniform.cdf(midpoint)
    Math.abs(cdf - 0.5) should be < epsilon

    // Mean is midpoint
    Math.abs(uniform.mean - midpoint) should be < epsilon
  }
}

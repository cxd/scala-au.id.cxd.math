package function.gamma

import au.id.cxd.math.function.gamma.{IncompleteGamma, InverseGamma}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Inverse Incomplete Gamma function
  *
  * Tests include:
  * - Round-trip property
  * - Edge cases
  * - Monotonicity
  * - Relationship with IncompleteGamma
  */
class TestInverseGammaComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-6

  "Inverse gamma P" should "satisfy round-trip property for p=0.1, a=1" in {
    val p = 0.1
    val a = 1.0

    val x = InverseGamma.P(p, a, 1.0)
    val pBack = IncompleteGamma.P(a, x)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse gamma P" should "satisfy round-trip property for p=0.5, a=1" in {
    val p = 0.5
    val a = 1.0

    val x = InverseGamma.P(p, a, 1.0)
    val pBack = IncompleteGamma.P(a, x)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse gamma P" should "satisfy round-trip property for p=0.9, a=1" in {
    val p = 0.9
    val a = 1.0

    val x = InverseGamma.P(p, a, 1.0)
    val pBack = IncompleteGamma.P(a, x)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse gamma P" should "satisfy round-trip property for p=0.5, a=2" in {
    val p = 0.5
    val a = 2.0

    val x = InverseGamma.P(p, a, 1.0)
    val pBack = IncompleteGamma.P(a, x)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse gamma P" should "satisfy round-trip property for p=0.5, a=5" in {
    val p = 0.5
    val a = 5.0

    val x = InverseGamma.P(p, a, 1.0)
    val pBack = IncompleteGamma.P(a, x)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse gamma P" should "satisfy round-trip property for various p and a" in {
    val testCases = List(
      (0.1, 1.0),
      (0.5, 1.0),
      (0.9, 1.0),
      (0.5, 2.0),
      (0.5, 5.0),
      (0.25, 5.0),
      (0.75, 5.0)
    )

    testCases.foreach { case (p, a) =>
      val x = InverseGamma.P(p, a, 1.0)
      val pBack = IncompleteGamma.P(a, x)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "InverseGamma with IncompleteGamma" should "form identity for x > 0" in {
    val a = 2.0

    val testX = List(0.5, 1.0, 2.0, 4.0)

    testX.foreach { x =>
      val p = IncompleteGamma.P(a, x)
      // Skip very small p values as they have poor numerical precision
      if (p >= 1e-9) {
        val xBack = InverseGamma.P(p, a, 1.0)
        Math.abs(xBack - x) should be < 1e-4
      }
    }
  }

  "Inverse gamma P" should "be monotonically increasing in p" in {
    val a = 2.0

    val x1 = InverseGamma.P(0.2, a, 1.0)
    val x2 = InverseGamma.P(0.5, a, 1.0)
    val x3 = InverseGamma.P(0.8, a, 1.0)

    x1 should be < x2
    x2 should be < x3
  }

  "Inverse gamma P" should "return positive values" in {
    val testCases = List(
      (0.1, 1.0),
      (0.5, 2.0),
      (0.9, 5.0)
    )

    testCases.foreach { case (p, a) =>
      val x = InverseGamma.P(p, a, 1.0)
      x should be > 0.0
    }
  }

  "Inverse gamma Q" should "satisfy InverseGammaQ(q) = InverseGammaP(1-q)" in {
    val q = 0.1
    val a = 2.0

    val x1 = InverseGamma.Q(q, a, 1.0)
    val x2 = InverseGamma.P(1.0 - q, a, 1.0)

    Math.abs(x1 - x2) should be < 1e-10
  }

  "Inverse gamma with scale parameter" should "scale the result" in {
    val p = 0.5
    val a = 2.0
    val scale = 2.0

    val x1 = InverseGamma.P(p, a, 1.0)
    val x2 = InverseGamma.P(p, a, scale)

    Math.abs(x2 - x1 * scale) should be < epsilon
  }

  "Inverse gamma for small p" should "give small x" in {
    val x = InverseGamma.P(0.01, 2.0, 1.0)
    x should be > 0.0
    x should be < 1.0
  }

  "Inverse gamma for large p" should "give large x" in {
    val x = InverseGamma.P(0.99, 2.0, 1.0)
    x should be > 3.0
  }
}

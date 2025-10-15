package function.beta

import au.id.cxd.math.function.beta.{IncompleteBetaFn, InverseBeta}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Inverse Incomplete Beta function
  *
  * Tests include:
  * - Round-trip property
  * - Known values
  * - Edge cases
  * - Monotonicity
  */
class TestInverseBetaComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-6

  "Inverse beta" should "satisfy round-trip property for p=0.1, α=2, β=3" in {
    val p = 0.1
    val alpha = 2.0
    val beta = 3.0

    val x = InverseBeta(p, alpha, beta)
    val pBack = IncompleteBetaFn(x, alpha, beta)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse beta" should "satisfy round-trip property for p=0.5, α=2, β=3" in {
    val p = 0.5
    val alpha = 2.0
    val beta = 3.0

    val x = InverseBeta(p, alpha, beta)
    val pBack = IncompleteBetaFn(x, alpha, beta)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse beta" should "satisfy round-trip property for p=0.9, α=2, β=3" in {
    val p = 0.9
    val alpha = 2.0
    val beta = 3.0

    val x = InverseBeta(p, alpha, beta)
    val pBack = IncompleteBetaFn(x, alpha, beta)

    Math.abs(pBack - p) should be < epsilon
  }

  "Inverse beta for symmetric distribution" should "return 0.5 when p=0.5" in {
    val result = InverseBeta(0.5, 5.0, 5.0)
    Math.abs(result - 0.5) should be < epsilon
  }

  "Inverse beta" should "satisfy round-trip for various p values" in {
    val testCases = List(
      (0.1, 2.0, 3.0),
      (0.5, 2.0, 3.0),
      (0.9, 2.0, 3.0),
      (0.25, 5.0, 5.0),
      (0.5, 5.0, 5.0),
      (0.75, 5.0, 5.0)
    )

    testCases.foreach { case (p, alpha, beta) =>
      val x = InverseBeta(p, alpha, beta)
      val pBack = IncompleteBetaFn(x, alpha, beta)
      Math.abs(pBack - p) should be < epsilon
    }
  }

  "Inverse beta" should "be monotonically increasing in p" in {
    val alpha = 2.0
    val beta = 3.0

    val x1 = InverseBeta(0.2, alpha, beta)
    val x2 = InverseBeta(0.5, alpha, beta)
    val x3 = InverseBeta(0.8, alpha, beta)

    x1 should be < x2
    x2 should be < x3
  }

  "Inverse beta" should "return values in (0,1)" in {
    val testCases = List(
      (0.1, 2.0, 3.0),
      (0.5, 2.0, 3.0),
      (0.9, 2.0, 3.0),
      (0.25, 5.0, 5.0),
      (0.75, 5.0, 5.0)
    )

    testCases.foreach { case (p, alpha, beta) =>
      val x = InverseBeta(p, alpha, beta)
      x should be > 0.0
      x should be < 1.0
    }
  }

  "InverseBeta with IncompleteBeta" should "form identity for x in (0,1)" in {
    val alpha = 2.0
    val beta = 3.0

    val testX = List(0.2, 0.5, 0.8)

    testX.foreach { x =>
      val p = IncompleteBetaFn(x, alpha, beta)
      val xBack = InverseBeta(p, alpha, beta)
      Math.abs(xBack - x) should be < epsilon
    }
  }

  "Inverse beta for small p" should "give small x" in {
    val x = InverseBeta(0.01, 2.0, 3.0)
    x should be > 0.0
    x should be < 0.2
  }

  "Inverse beta for large p" should "give large x" in {
    val x = InverseBeta(0.99, 2.0, 3.0)
    x should be > 0.8
    x should be < 1.0
  }

  "Inverse beta Q function" should "satisfy InverseBetaQ(q) = InverseBetaP(1-q)" in {
    val q = 0.1
    val alpha = 2.0
    val beta = 3.0

    val x1 = InverseBeta().beta_Qinv(q, alpha, beta)
    val x2 = InverseBeta(1.0 - q, alpha, beta)

    Math.abs(x1 - x2) should be < 1e-10
  }
}

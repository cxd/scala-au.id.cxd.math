package function.beta

import au.id.cxd.math.function.beta.IncompleteBetaFn
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Regularized Incomplete Beta function I_x(α,β)
  *
  * Tests include:
  * - Edge cases (x=0, x=1)
  * - Known values
  * - Symmetry property
  * - Monotonicity
  */
class TestIncompleteBetaFnComprehensive extends FlatSpec with Matchers {

  val fn = IncompleteBetaFn(_, _, _)
  val epsilon = 1e-4

  "Regularized incomplete beta I_0(α,β)" should "equal 0" in {
    val result = fn(0.0, 2.0, 3.0)
    Math.abs(result - 0.0) should be < 1e-15
  }

  "Regularized incomplete beta I_1(α,β)" should "equal 1" in {
    val result = fn(1.0, 2.0, 3.0)
    Math.abs(result - 1.0) should be < 1e-15
  }

  "Regularized incomplete beta I_0.3(2,3)" should "match known value" in {
    val result = fn(0.3, 2.0, 3.0)
    val expected = 0.3483
    Math.abs(result - expected) should be < epsilon
  }

  "Regularized incomplete beta I_0.5(2,3)" should "match known value" in {
    val result = fn(0.5, 2.0, 3.0)
    val expected = 0.6875
    Math.abs(result - expected) should be < epsilon
  }

  "Regularized incomplete beta I_0.7(2,3)" should "match known value" in {
    val result = fn(0.7, 2.0, 3.0)
    val expected = 0.9163
    Math.abs(result - expected) should be < epsilon
  }

  "Regularized incomplete beta I_0.5(5,5)" should "equal 0.5 (symmetry)" in {
    val result = fn(0.5, 5.0, 5.0)
    Math.abs(result - 0.5) should be < epsilon
  }

  "Regularized incomplete beta" should "satisfy symmetry I_x(α,β) = 1 - I_(1-x)(β,α)" in {
    val testCases = List(
      (0.3, 2.0, 3.0),
      (0.5, 2.0, 3.0),
      (0.7, 2.0, 3.0),
      (0.2, 5.0, 5.0),
      (0.8, 5.0, 5.0)
    )

    testCases.foreach { case (x, alpha, beta) =>
      val i1 = fn(x, alpha, beta)
      val i2 = fn(1.0 - x, beta, alpha)
      val sum = i1 + i2
      Math.abs(sum - 1.0) should be < 1e-12
    }
  }

  "Regularized incomplete beta" should "be monotonically increasing in x" in {
    val alpha = 2.0
    val beta = 3.0

    val x1 = 0.3
    val x2 = 0.5
    val x3 = 0.7

    val i1 = fn(x1, alpha, beta)
    val i2 = fn(x2, alpha, beta)
    val i3 = fn(x3, alpha, beta)

    i1 should be < i2
    i2 should be < i3
  }

  "Regularized incomplete beta for α=1" should "equal 1-(1-x)^β" in {
    val x = 0.5
    val beta = 3.0

    val result = fn(x, 1.0, beta)
    val expected = 1.0 - Math.pow(1.0 - x, beta)

    Math.abs(result - expected) should be < epsilon
  }

  "Regularized incomplete beta for β=1" should "equal x^α" in {
    val x = 0.5
    val alpha = 3.0

    val result = fn(x, alpha, 1.0)
    val expected = Math.pow(x, alpha)

    Math.abs(result - expected) should be < epsilon
  }

  "Regularized incomplete beta" should "handle small x values" in {
    val result = fn(0.01, 2.0, 3.0)
    result should be > 0.0
    result should be < 0.1
  }

  "Regularized incomplete beta" should "handle large x values" in {
    val result = fn(0.99, 2.0, 3.0)
    result should be > 0.9
    result should be < 1.0
  }
}

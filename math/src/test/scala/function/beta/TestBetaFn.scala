package function.beta

import au.id.cxd.math.function.beta.BetaFn
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Beta function B(α,β)
  *
  * Tests include:
  * - Known values
  * - Symmetry property
  * - Special cases
  */
class TestBetaFn extends FlatSpec with Matchers {

  val epsilon = 1e-14

  "Beta function B(1,1)" should "equal 1" in {
    val result = BetaFn(1.0)(1.0)
    Math.abs(result - 1.0) should be < epsilon
  }

  "Beta function B(2,1)" should "equal 0.5" in {
    val result = BetaFn(2.0)(1.0)
    Math.abs(result - 0.5) should be < epsilon
  }

  "Beta function B(1,2)" should "equal 0.5" in {
    val result = BetaFn(1.0)(2.0)
    Math.abs(result - 0.5) should be < epsilon
  }

  "Beta function B(2,2)" should "equal 1/6" in {
    val result = BetaFn(2.0)(2.0)
    val expected = 1.0 / 6.0
    Math.abs(result - expected) should be < epsilon
  }

  "Beta function B(2,3)" should "equal 1/12" in {
    val result = BetaFn(2.0)(3.0)
    val expected = 1.0 / 12.0
    Math.abs(result - expected) should be < epsilon
  }

  "Beta function B(3,2)" should "equal 1/12" in {
    val result = BetaFn(3.0)(2.0)
    val expected = 1.0 / 12.0
    Math.abs(result - expected) should be < epsilon
  }

  "Beta function B(5,5)" should "equal 1/630" in {
    val result = BetaFn(5.0)(5.0)
    val expected = 1.0 / 630.0
    Math.abs(result - expected) should be < epsilon
  }

  "Beta function B(0.5,0.5)" should "equal π" in {
    val result = BetaFn(0.5)(0.5)
    Math.abs(result - Math.PI) should be < 1e-13
  }

  "Beta function" should "satisfy symmetry B(α,β) = B(β,α)" in {
    val testCases = List(
      (1.0, 2.0),
      (2.0, 3.0),
      (3.0, 5.0),
      (0.5, 1.5),
      (10.0, 20.0)
    )

    testCases.foreach { case (a, b) =>
      val b1 = BetaFn(a)(b)
      val b2 = BetaFn(b)(a)
      Math.abs(b1 - b2) should be < epsilon
    }
  }

  "Beta function for large parameters" should "not overflow" in {
    val result = BetaFn(100.0)(200.0)
    result should be > 0.0
    result.isInfinite should be (false)
    result.isNaN should be (false)
  }

  "Beta function recursion" should "satisfy B(α+1,β) = (α/(α+β)) * B(α,β)" in {
    val alpha = 3.0
    val beta = 4.0

    val b1 = BetaFn(alpha + 1.0)(beta)
    val b2 = (alpha / (alpha + beta)) * BetaFn(alpha)(beta)

    Math.abs(b1 - b2) should be < 1e-12
  }
}

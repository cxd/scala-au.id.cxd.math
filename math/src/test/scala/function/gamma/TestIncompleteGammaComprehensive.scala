package function.gamma

import au.id.cxd.math.function.gamma.IncompleteGamma
import org.scalatest.{FlatSpec, Matchers}

/**
  * Comprehensive tests for Incomplete Gamma functions P(a,x) and Q(a,x)
  *
  * Tests include:
  * - Edge cases
  * - Known values
  * - Complement property P + Q = 1
  * - Monotonicity
  */
class TestIncompleteGammaComprehensive extends FlatSpec with Matchers {

  val epsilon = 1e-9

  "Incomplete gamma P(a,0)" should "equal 0" in {
    val result = IncompleteGamma.P(1.0, 0.0)
    Math.abs(result - 0.0) should be < 1e-15
  }

  "Incomplete gamma Q(a,0)" should "equal 1" in {
    val result = IncompleteGamma.Q(1.0, 0.0)
    Math.abs(result - 1.0) should be < 1e-15
  }

  "Incomplete gamma P(1,1)" should "match known value" in {
    val result = IncompleteGamma.P(1.0, 1.0)
    val expected = 0.632120558828558
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma P(2,2)" should "match known value" in {
    val result = IncompleteGamma.P(2.0, 2.0)
    val expected = 0.5939941502901617
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma P(5,5)" should "match known value" in {
    val result = IncompleteGamma.P(5.0, 5.0)
    val expected = 0.5595067149315669
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma P(2,0.5)" should "match known value" in {
    val result = IncompleteGamma.P(2.0, 0.5)
    val expected = 0.09020401043104985
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma P(2,4)" should "match known value" in {
    val result = IncompleteGamma.P(2.0, 4.0)
    val expected = 0.908421805556329
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma Q(1,1)" should "match known value" in {
    val result = IncompleteGamma.Q(1.0, 1.0)
    val expected = 0.36787944117144233
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma Q(2,2)" should "match known value" in {
    val result = IncompleteGamma.Q(2.0, 2.0)
    val expected = 0.40600584970983833
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma Q(5,5)" should "match known value" in {
    val result = IncompleteGamma.Q(5.0, 5.0)
    val expected = 0.44049328506843313
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma" should "satisfy P(a,x) + Q(a,x) = 1" in {
    val testCases = List(
      (1.0, 1.0),
      (2.0, 2.0),
      (5.0, 5.0),
      (2.0, 0.5),
      (2.0, 4.0),
      (10.0, 8.0)
    )

    testCases.foreach { case (a, x) =>
      val p = IncompleteGamma.P(a, x)
      val q = IncompleteGamma.Q(a, x)
      val sum = p + q
      Math.abs(sum - 1.0) should be < 1e-14
    }
  }

  "Incomplete gamma P" should "be monotonically increasing in x" in {
    val a = 2.0

    val p1 = IncompleteGamma.P(a, 0.5)
    val p2 = IncompleteGamma.P(a, 2.0)
    val p3 = IncompleteGamma.P(a, 4.0)

    p1 should be < p2
    p2 should be < p3
  }

  "Incomplete gamma Q" should "be monotonically decreasing in x" in {
    val a = 2.0

    val q1 = IncompleteGamma.Q(a, 0.5)
    val q2 = IncompleteGamma.Q(a, 2.0)
    val q3 = IncompleteGamma.Q(a, 4.0)

    q1 should be > q2
    q2 should be > q3
  }

  "Incomplete gamma P" should "approach 1 for large x" in {
    val result = IncompleteGamma.P(2.0, 20.0)
    result should be > 0.999
  }

  "Incomplete gamma Q" should "approach 0 for large x" in {
    val result = IncompleteGamma.Q(2.0, 20.0)
    result should be < 0.001
  }

  "Incomplete gamma for a=1" should "equal 1-exp(-x)" in {
    val x = 2.0
    val result = IncompleteGamma.P(1.0, x)
    val expected = 1.0 - Math.exp(-x)
    Math.abs(result - expected) should be < epsilon
  }

  "Incomplete gamma" should "handle small a and x" in {
    val result = IncompleteGamma.P(0.5, 0.1)
    result should be > 0.0
    result should be < 1.0
    result.isNaN should be (false)
  }

  "Incomplete gamma" should "handle large a and x" in {
    val result = IncompleteGamma.P(50.0, 50.0)
    result should be > 0.4
    result should be < 0.6
    result.isNaN should be (false)
  }
}

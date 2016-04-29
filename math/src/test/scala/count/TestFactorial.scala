package count

import au.id.cxd.math.count.Factorial
import org.scalatest._

/**
 * Check that the factorial operation outputs the same as factorial implemented in R for domain 1..10
 *
 * Created by cd on 5/11/14.
 */
class TestFactorial extends FlatSpec with ShouldMatchers {

  val x = List(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0)
  val testResults = List(1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0, 40320.0, 362880.0, 3628800.0)

  "Factorial" should "match R factorial results" in {

    val y = x map Factorial.apply
    val delta = y.zip(testResults) map { (pair) => pair._1 - pair._2 }
    println(y)
    delta.sum should be(0.0)
  }
}

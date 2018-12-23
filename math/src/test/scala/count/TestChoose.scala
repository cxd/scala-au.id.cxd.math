package count

import au.id.cxd.math.count.Choose
import org.scalatest._

/**
 * Check that the choose operation outputs the same results as those implemented by R for domain 1..10
 *
 * Created by cd on 5/11/14.
 */
class TestChoose  extends FlatSpec with Matchers {

  val x = List(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0)
  val testResults = List(10.0, 45.0, 120.0, 210.0, 252.0, 210.0, 120.0,  45.0,  10.0,  1.0)

  "Choose" should "math R results" in {
    val y = x map Choose.apply(10.0)
    val delta = (y zip testResults) map { (pair) => pair._1 - pair._2 }
    delta.sum should be(0.0)
  }

}

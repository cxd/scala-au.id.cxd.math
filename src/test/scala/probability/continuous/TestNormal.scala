package probability.continuous

import au.id.cxd.math.probability.continuous.Normal
import org.scalatest._
import scala.collection.immutable.NumericRange
import scala.collection.immutable.Range.Double
/**
 * Created by cd on 13/11/14.
 */
class TestNormal extends FlatSpec with ShouldMatchers {

  val range:NumericRange[Double] = Range.Double.apply(-4.0, 4.0, 0.1)

  "Normal" should "have pdf" in {
    val norm = Normal(0.0)(1.0)
    val y = range map norm.pdf
    println(y)
    val total = norm cdf range
    println("Total: " + total)
  }
}

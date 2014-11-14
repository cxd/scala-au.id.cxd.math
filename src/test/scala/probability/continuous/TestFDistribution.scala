package probability.continuous

import au.id.cxd.math.probability.continuous.FDistribution
import org.scalatest._

import scala.collection.immutable.NumericRange

/**
 * Created by cd on 13/11/14.
 */
class TestFDistribution extends FlatSpec with ShouldMatchers {

  val range:NumericRange[Double] = Range.Double.apply(0.1, 4.0, 0.1)


  "FDistribution" should "have pdf" in {
    val fdist = FDistribution(1.0, 2.0)
    val y = range map fdist.pdf
    println("Test: " + y)
    val total = fdist cdf range

    println("Total: " + total)

  }

}

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

  val quantiles = List[Double](
    -1.281552,
    -0.8416212,
    -0.5244005,
    -0.2533471,
    0,
    0.2533471,
    0.5244005,
    0.8416212,
    1.281552
  )
  val percent = List[Double](
    0.1,
    0.2,
    0.3,
    0.4,
    0.5,
    0.6,
    0.7,
    0.8,
    0.9
  )


  "Normal cdf" should "agree with R" in {
    val norm = Normal(0.0)(1.0)
    val y = quantiles map norm.cdf
    val test = percent.zip (y)
    test.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
    test.forall {
      pair => Math.abs(pair._1 - pair._2) < 0.1
    } should be (true)
  }

  "Normal invcdf" should "agree with R quantiles" in {

    val norm = Normal(0.0)(1.0)
    val results = percent.map (norm.invcdf(_))
    val test = quantiles.zip (results)
    test.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
    test.forall {
      pair => Math.abs(pair._1 - pair._2) < 0.1
    } should be (true)
  }
}

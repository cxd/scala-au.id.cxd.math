package probability.continuous

import au.id.cxd.math.probability.continuous.FDistribution
import org.scalatest._

import scala.collection.immutable.NumericRange

/**
 * Created by cd on 13/11/14.
 */
class TestFDistribution extends FlatSpec with ShouldMatchers {

  val range:NumericRange[Double] = Range.Double.apply(0.1, 4.0, 0.1)

  /*
  > x <- seq(from = 0.1, to = 4, by = 0.1)
> y <- df(x, 1.0, 2.0)
> y
 [1] 1.03913281 0.68525306 0.52341675 0.42525864 0.35777088 0.30793876 0.26940480 0.23862611 0.21344292
[10] 0.19245009 0.17468721 0.15947199 0.14630441 0.13480860 0.12469594 0.11574074 0.10776381 0.10062094
[19] 0.09419466 0.08838835 0.08312173 0.07832758 0.07394919 0.06993835 0.06625387 0.06286032 0.05972709
[28] 0.05682758 0.05413858 0.05163978 0.04931328 0.04714330 0.04511586 0.04321852 0.04144021 0.03977102
[37] 0.03820207 0.03672537 0.03533375 0.03402069
   */

  "FDistribution" should "have pdf" in {
    val fdist = FDistribution(1.0, 2.0)
    val y = range map fdist.pdf
    println("Test: " + y)
    val total = fdist cdf range

    println("Total: " + total)

  }

}

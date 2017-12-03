package probability.continuous

import au.id.cxd.math.function.beta.BetaFn
import au.id.cxd.math.function.gamma.GammaFn
import au.id.cxd.math.probability.analysis.CriticalValue
import au.id.cxd.math.probability.continuous.FDistribution
import org.scalatest._

import scala.collection.immutable.NumericRange

/**
 * A simple comparison with output from R.
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

  val numeratorDf = 2.0
  val denominatorDf = 2.0

  val beta = BetaFn(numeratorDf/2.0)(denominatorDf/2.0)

  def methodA(y:Double) = {
    val a = Math.pow(numeratorDf*y, numeratorDf)*Math.pow(denominatorDf, denominatorDf)
    val b = Math.pow(numeratorDf*y + denominatorDf, numeratorDf+denominatorDf)
    val c = Math.sqrt(a / b)
    val d = y * beta
    c / d
  }

  def methodB(y:Double) = {
    val a = GammaFn(numeratorDf/2.0 + denominatorDf/2.0)
    val b = GammaFn(numeratorDf/2.0) * GammaFn(denominatorDf/2.0)
    val c = Math.pow(numeratorDf/denominatorDf, numeratorDf/2.0)
    val d = Math.pow(y, numeratorDf/(2.0)-1)
    val e = Math.pow(1 + (numeratorDf*y / denominatorDf), -1.0*(numeratorDf+denominatorDf)/2.0)
    a/b *c *d * e
  }

  "Compare Methods" should "equal" in {
    val testA = CriticalValue.sequence(1,by=1).take(10)
    val testB = CriticalValue.sequence(1,by=1).take(10)
    val resultA = testA.map(methodA).toList
    val resultB = testB.map(methodB).toList
    resultA.map(println)
    println()
    resultB.map(println)
    resultA.zip(resultB).map(pair => pair._1 == pair._2)
  }

}
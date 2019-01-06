package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.distance.Cov
import au.id.cxd.math.function.moments.{Kurtosis, StdDeviation, Variance}
import au.id.cxd.math.function.series.PolyVal
import au.id.cxd.math.probability.continuous.Normal
import breeze.linalg.DenseVector

/**
  * Shapiro Wilks
  * Univariate test for normality
  *
  * https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
  *
  *
  *
  *
  * See:
  * https://ww2.mathworks.cn/matlabcentral/mlc-downloads/downloads/submissions/13964/versions/2/previews/swtest.m/index.html
  *
  * as well as:
  * https://github.com/wch/r-source/blob/trunk/src/library/stats/src/swilk.c
  *
  * Both reference the AS R94 article on the shapiro wilk normality test.
  * The R source version swilk.c is converted from the fortran implementation of that algorithm.
  * The matlab source being a DSL is much easier to read through.
  * However in both cases the coefficients of the polynomials are provided in tables and these are used in the expansion when
  * calculating the a_i coefficients in the statistic.
  *
  * In order to provide an implementation, the same constant coefficients will be used.
  * However simplification of the C version is attempted with heavy influence from the approach taken in matlab.
  *
  *
  */
class ShapiroWilksTest(val series: Seq[Double]) extends StatisticalTest {

  /* polynomial coefficients
  * Defined in swilk.c line 49.
  * Defined in swtest.m line 164
  *
  * The first position in c1 and c2 will be replaced by a new value during the calculation.
  * */
  val c1: Seq[Double] = Array(0.0, .221157, -.147981, -2.07119, 4.434685, -2.706056)
  val c2: Seq[Double] = Array(0.0, .042981, -.293762, -1.752461, 5.682633, -3.582633)
  val c3: Seq[Double] = Array(.544, -.39978, .025054, -6.714e-4)
  val c4: Seq[Double] = Array(1.3822, -.77857, .062767, -.0020322)
  val c5: Seq[Double] = Array(-1.5861, -.31082, -.083751, .0038915)
  val c6: Seq[Double] = Array(-.4803, -.082676, .0030302)
  val c7: Seq[Double] = Array(-2.273, .459)

  /**
    * normal distribution.
    */
  val norm = Normal(mu = 0.0)(1.0)

  /**
    * the ordered function.
    *
    * @return
    */
  def ordered(): Seq[Double] = series.sorted


  def indexed(set: Seq[Double]): Seq[(Int, Double)] = {
    val indices: Seq[Int] = for (i <- 1 to set.length) yield i
    indices.zip(set)
  }

  /**
    * approximate the mean of ith order statistics for quantiles 1:n
    *
    * @param n
    */
  def mQuantiles(n: Int): DenseVector[Double] =
    DenseVector.tabulate[Double](n) {
      case i => {
        norm.invcdf((i + 1 - 3.0 / 8.0) / (n + 1.0 / 4.0))
      }
    }

  def zeros(n: Int): Seq[Double] =
    for (i <- 1 to n) yield 0.0


  /**
    * Compute the shapiro francia statistic and the corresponding pvalue
    *
    * @param x
    * @return tuple of (critical value x p value)
    */
  def shapiroFrancia(x: Seq[Double]): (Double, Double) = {
    val n = x.length
    val mtilde1 = mQuantiles(n)
    val mtilde: Seq[Double] = mtilde1.data
    val mdot = mtilde1.dot(mtilde1)
    // apply shapiro francia test.
    val weights1 = 1.0 / Math.sqrt(mdot)
    val weights = mtilde.map { m => m * weights1 }
    // calculate W = Cov(x,m) / \sigma_x \sigma_w
    val sigmaX = StdDeviation(x)
    val sigmaW = StdDeviation(weights)
    val W = Cov(x, weights) / (sigmaX * sigmaW)
    val nu = Math.log(n)
    val u1 = Math.log(nu) - nu
    val u2 = Math.log(nu) + 2.0 / nu
    val mu = -1.2725 + (1.0521 * u1)
    val sigma = 1.0308 - (0.26758 * u2)
    val statTemp = Math.log(1 - W)
    val normalisedStat = (statTemp - mu) / sigma
    val pVal = 1.0 - norm.cdf(normalisedStat)
    (normalisedStat, pVal)
  }


  /**
    * Compute the shapiro wilks statistic and corresponding pvalue
    *
    * @param x
    * @return tuple of (critival value x p value)
    */
  def shapiroWilks(x: Seq[Double]): (Double, Double) = {
    val n = x.length
    val mtilde1 = mQuantiles(n)
    val mtilde: Seq[Double] = mtilde1.data
    val mdot = mtilde1.dot(mtilde1)
    // apply shapiro francia test.
    val weights1 = 1.0 / Math.sqrt(mdot)
    val c = mtilde.map { m => m * weights1 }
    val u = 1.0 / Math.sqrt(n)
    // update the coefficients
    val m = c.last
    val polyCoef1 = Seq(m) ++ c1.tail
    val polyCoef2 = Seq(m) ++ c2.tail

    // calculating the weights has a number of special cases.
    // n > 5
    // In this case the indexes 1,2, and n-1,n are precomputed in the weights.
    // we fill the range between 3 and n-2
    // n < 5
    // n == 3
    // in these cases the indexes 1 and n are precomputed
    // we fill the rnage between 2 and n-1
    // We end up with Seq(start ++ middle ++ end)
    val weights: Seq[Double] = if (n > 5) {
      val wN = PolyVal(polyCoef1.toList, u)
      val w1 = -1.0 * wN

      val wN1 = PolyVal(polyCoef2.toList, u)
      val w2 = -1.0 * wN1

      val m1 = mtilde(n - 1)
      val m2 = mtilde(n - 2)


      val phi = (mdot - 2 * m1 * m1 - 2 * m2 * m2) / (1.0 - 2.0 * wN * wN - 2.0 * wN1 * wN1)

      val startW = Seq(w1, w2)
      val endW = Seq(wN1, wN)

      // fill from 2 to n-2
      val phi1 = Math.sqrt(phi)
      val wMid = for (i <- 2 until n - 2) yield mtilde(i) / phi1

      startW ++ wMid ++ endW

    } else if (n == 3) {
      val w1 = 1.0 / Math.sqrt(2.0)
      val wN = -1.0 * w1
      val phi = 1.0

      val startW = Seq(w1)
      val endW = Seq(wN)

      // fill from 2 to n-1

      val phi1 = Math.sqrt(phi)
      val wMid = for (i <- 1 to n - 1) yield mtilde(i) / phi1

      startW ++ wMid ++ endW
    } else {
      val wN = PolyVal(polyCoef1.toList, u)
      val w1 = -1.0 * wN
      val m1 = mtilde(n - 1)
      val phi = (mdot - 2.0 * m1 * m1) / (1.0 - 2.0 * wN * wN)

      val startW = Seq(w1)
      val endW = Seq(wN)

      // fill from 2 to n-1
      val phi1 = Math.sqrt(phi)
      val wMid = for (i <- 1 to n - 1) yield mtilde(i) / phi1

      startW ++ wMid ++ endW
    }
    // calculate W = Cov(x,m) / \sigma_x \sigma_w
    val X = DenseVector.tabulate[Double](x.length) { case i => x(i) }
    val w = DenseVector.tabulate[Double](weights.length) { case i => weights(i) }
    val variance = n * Variance(x)
    val W = (w.dot(X) * w.dot(X)) / variance

    // there are special cases for estimating the adjusted statistic, its means and standard deviation.
    val normalStat =
      if (n == 3) {
        val mu = 0.0
        val sigma = 1.0
        val stat = 0.0
        0.0
      } else if (n >= 4 && n <= 11) {
        val mu = PolyVal(c3.toList, n)
        val sigma = Math.exp(PolyVal(c4.toList, n))
        val gam = PolyVal(c7.toList, n)

        val stat = -1.0 * Math.log(gam - Math.log(1.0 - W))

        (stat - mu) / sigma
      } else {
        val newn = Math.log(n)
        val mu = PolyVal(c5.toList, newn)
        val sigma = Math.exp(PolyVal(c6.toList, newn))

        val stat = if (W > 1.0) 0.0
          else Math.log(1.0 - W)

        (stat - mu) / sigma
      }

    val pVal = if (n == 3) {
      6.0 / Math.PI * (Math.asin(Math.sqrt(W)) - Math.asin(Math.sqrt(3.0 / 4.0)))
    } else
      1.0 - norm.cdf(normalStat)


    (normalStat, pVal)
  }

  /**
    * Compute the critical value of the test statistic.
    *
    * @return Returns a tuple (critical value x p value)
    */
  def computeStat(): (Double, Double) = {
    val x = ordered()

    val k = Kurtosis(x)

    val (statistic, pval) =
      if (k > 3)
        shapiroFrancia(x)
      else
        shapiroWilks(x)

    (statistic, pval)
  }

  override def test(alpha: Double): TestResult = {
    val (statistic, pval) = computeStat()
    val message =
      if (series.length > 5000) "Shapiro Wilks test may be inaccurate for large sample sizes above 5000"
      else ""
    val result = TestResult(
      name = "Shapiro Wilks Test",
      significance = alpha,
      reject = pval <= alpha,
      pValue = pval,
      observedValue = statistic,
      criticalValue = Math.abs(norm.invcdf(alpha)), // two tailed
      message = message)

    result

  }
}

object ShapiroWilksTest {
  def apply(series: Seq[Double]) = new ShapiroWilksTest(series)
}

package au.id.cxd.math.probability.analysis


import scala.Function1
import Stream.cons
import collection.immutable._
import au.id.cxd.math.probability.continuous.FDistribution
import breeze.linalg.{DenseVector, DenseMatrix}

/**
 *
 * $$ Total SS = \sum_{i=1}^k\sum_{j=1}^{n_i} (Y_{ij} - \bar{Y})^2 =  \sum_{i=1}^k\sum_{j=1}^{n_i}Y_{ij}^2 - CM $$
 *
 * $$ CM = \frac{1}{n} ( \sum_{i=1}^k\sum_{j=1}^{n_i} Y_{ij} ) ^2 $$
 *
 * $$ Y_i. = \sum_{j=1}^{n_i} Y_{ij} $$
 * $$
 * \bar{Y_{i.} } = \frac{1}{n_i} \sum_{j=1}^n_i Y_{ij} =  \frac{1}{n_i} Y_{i.}
 *
 * $$
 * $$
 *
 * SST = \sum_{i=1}^k n_i (\bar{Y_{i.} } - \bar{Y} )^2  = \sum_{i=1}^k \frac{Y_{i.}^2}{n_i} - CM
 *
 * $$
 * $$
 *
 * SSE = Total SS - SST
 *
 * $$
 * $$
 *
 * MSE = \frac{SSE}{n-k}
 *
 * $$
 * $$
 *
 * MST = \frac{SST}{k-1}
 *
 * $$
 * $$
 *
 * F = \frac{MST}{MSE} > F_\alpha
 *
 * $
 * Created by cd on 17/09/2014.
 */
class Anova(val X: DenseMatrix[Double]) extends StatisticalTest {

  /**
   * internal class for intermediate results
   * @param cm
   * @param totalSS
   * @param sst
   * @param sse
   * @param mst
   * @param mse
   */
  private class Intermediate(val cm:Double, val totalSS:Double, val sst:Double, val sse:Double, val mst:Double, val mse:Double) {}

  /**
   * f distribution
   * n = rows * cols
   * k = cols
   *
   * df = (k-1), (n-k)
   *
   */
  val fdist = FDistribution(X.cols - 1, X.cols * X.rows - X.cols)

  val criticalVal = CriticalValue(fdist.cdf, UpperTail()) _

  /**
   * the correction for the mean
   * @return
   */
  private def cm(): Intermediate = {
    val sum = X.toArray.foldLeft(0.0) {
      (n, d) => n + d
    }
    val n = X.rows * X.cols
    val cm = (1.0 / n.toDouble) * Math.pow(sum, 2.0)
    new Intermediate(cm, 0.0, 0.0, 0.0, 0.0, 0.0)
  }

  /**
   * sum the supplied column in the matrix
   * @param col
   * @param M
   * @return
   */
  private def sumCol(col: Int, M: DenseMatrix[Double])(fn: Double => Double) = {
    val sum = M(::, col).foldLeft(0.0) {
      (n, d) => n + fn(d)
    }
    sum
  }

  /**
   * fold an operation accross a matrix and accumulate the output in a vector
   * @param M
   * @param fn
   * @return
   */
  private def foldColumns(M: DenseMatrix[Double])(fn: Double => Double): Seq[Double] = {
    val cols = M.cols
    val idx = (0 to (cols - 1))
    val sums = idx.map(i => sumCol(i, M)(fn))
    sums
  }

  /**
   * compute the total SS.
   * $$ Total SS = \sum_{i=1}^k\sum_{j=1}^{n_i} (Y_{ij} - \bar{Y})^2 =  \sum_{i=1}^k\sum_{j=1}^{n_i}Y_{ij}^2 - CM $$
   *
   * $$ CM = \frac{1}{n} ( \sum_{i=1}^k\sum_{j=1}^{n_i} Y_{ij} ) ^2  $$
   * @return
   */
  private def totalSS(accum:Intermediate):Intermediate = {
    val CM = accum.cm
    val rows = X.rows
    val cols = X.cols
    val sums = foldColumns(X) { (y: Double) => Math.pow(y, 2.0)}
    val total = sums.sum
    val totalSS = total - CM
    new Intermediate(CM, totalSS, 0.0, 0.0, 0.0, 0.0)
  }

  /**
   * sum of squares treatment
   * $$
   *
   * SST = \sum_{i=1}^k n_i (\bar{Y_{i.} } - \bar{Y} )^2  = \sum_{i=1}^k \frac{Y_{i.}^2}{n_i} - CM
   *
   * $$
   */
  private def ssTreatment(accum:Intermediate):Intermediate = {
    val CM = accum.cm
    val rows = X.rows
    val cols = X.cols
    val sums = foldColumns(X) { (y: Double) => y}
    val ss = sums.map {
      (y: Double) => Math.pow(y, 2.0) / rows
    }.sum
    val sstr = ss - CM
    new Intermediate(CM, accum.totalSS, sstr, 0.0, 0.0, 0.0)
  }

  /**
   * the sum of squares error
   * @return
   */
  private def sse(accum:Intermediate):Intermediate = {
    val sse = accum.totalSS - accum.sst
    new Intermediate(accum.cm, accum.totalSS, accum.sst, sse, 0.0, 0.0)
  }

  /**
   * the mean sum of squares
   * @return
   */
  private def mse(accum:Intermediate): Intermediate = {
    val k = X.cols
    val n = X.rows * X.cols
    val mse = accum.sse / (n - k)
    new Intermediate(accum.cm, accum.totalSS, accum.sst, accum.sse, 0.0, mse)
  }

  /**
   * the mean sum of squares statistic
   * @return
   */
  private def mst(accum:Intermediate): Intermediate = {
    val k = X.cols
    val mst = accum.sst / (k - 1.0)
    new Intermediate(accum.cm, accum.totalSS, accum.sst, accum.sse, mst, accum.mse)
  }

  /**
   * compute the F-statistic
   * this is the assertion that
   * $H_0: \mu_1 = \mu_2 = ... = \mu_k$
   * vs
   * $H_a: $ none of the means are equal.
   *
   * @return
   */
  private def statistic(): (Double, Intermediate) = {
    val k = X.cols
    val n = X.cols * X.rows
    val builder = PartialFunction[Intermediate,Intermediate](_)
    val result = builder {
      case i => mst(i)
    } compose builder {
      case i => mse(i)
    } compose builder {
      case i => sse(i)
    } compose builder {
      case i => ssTreatment(i)
    } compose builder {
      case i => totalSS(i)
    }
    val interim = result(cm)
    val F = interim.mst / interim.mse
    (F, interim)
  }

  /**
   * perform the anova test at the supplied critical level
   * @param alpha
   * @return
   */
  def test(alpha: Double): TestResult = {
    def sequence(last: Double): Stream[Double] = {
      last #:: sequence(last + 0.1)
    }
    val (stat, interim) = statistic()
    val n = X.cols * X.rows
    val k = X.cols
    // calculate the critical value F statistic for (k-1), (n-k) df at alpha level
    val critical = criticalVal(sequence(0.0).take(100))
    val test = critical.value(alpha)
    val reject = stat > test
    // upper tail
    val prob = 1.0 - fdist.integral(0.0, stat)
    return new AnovaTable(alpha,
      reject,
      prob,
      stat,
      test,
      k-1,
      n-k,
      interim.sst,
      interim.mst,
      interim.sse,
      interim.mse, n-1,
      interim.totalSS)
  }

}

object Anova {
  def apply(X: DenseMatrix[Double]) = new Anova(X)
}

class AnovaTable(/**
                  * significance
                  * test at the level of significance
                  * alpha
                  */
                 significance: Double,

                 /**
                  * determine whether $H_0$ can be rejected
                  */
                 reject: Boolean,

                 /**
                  * The p-value for the observed statistic
                  */
                 pValue: Double,

                 /**
                  * the observed value
                  */
                 observedValue: Double,

                 /**
                  * the critical value for the observed statistic.
                  */
                 criticalValue: Double,

                 /**
                  * k-1 df
                  */
                 numeratorDf: Int,

                 /**
                  * n-k df
                  */
                 denominatorDf: Int,

                 /**
                  * SSTr
                  */
                 ssTreatment: Double,

                 /**
                  * mean sum of square treatment
                  */
                 mst: Double,

                 /**
                  * sum of square error
                  */
                 sse: Double,

                 /**
                  * mean sum of square error
                  */
                 mse: Double,

                 /**
                  * total df
                  */
                 totalDf: Int,

                 /**
                  * Total SS
                  */
                 totalSS: Double

                  ) extends TestResult(significance, reject, pValue, observedValue, criticalValue) {

  override def toString() = {
    s"""NumeratorDF: $numeratorDf\nDenominatorDF: $denominatorDf\n""" +
    s"""SST: $ssTreatment\nSSE: $sse\n""" +
    s"""MSE: $mse\n MST: $mst\n"""+
    s"""TotalDF: $totalDf\nTotalSS: $totalSS\n"""+
    s"""F-stat (observed statistic): $observedValue\n"""+
    s"""F-alpha (critical value): $criticalValue\n"""+
    s"""P-Value: $pValue\n"""+
    s"""alpha (significance level):$significance"""

  }

}
package au.id.cxd.math.probability.analysis


import scala.Function1
import Stream.cons
import collection.immutable._
import au.id.cxd.math.probability.continuous.FDistribution
import breeze.linalg.{DenseVector, DenseMatrix}

/**
  *
  * ##import MathJax
  *
  * This class implements an Anova procedure as derived from 'Mathematical Statistics with Applications' 7th edition by Wackerly, et el.
  *
  * That resource provides a full explanation, however a summary is given below.
  *
  * The goal of Anova is to "identify the important independent variables and determine how they affect the response".
  *
  * Where each independent variable is a factor and the intensity of the variable the level.
  *
  * The procedure calculates the Total Sum of Squares which is the sum of the squared deviations of each
  * variable from the mean and a remainder random error.
  *
  * We consider that under the null hypothesis the independent variables are assumed to be unrelated
  * to the response variable, and each portion of the total sum of squares divided
  * by a corresponding constant provides an independent and unbiased estimator of $\sigma^2$ of the
  * experimental error.
  *
  * If a variable is highly related to the response its contribution to the total sum of squares
  * will be large.
  *
  * The variable sum of squares total $SST$ is compared with the sum of squares for the error $SSE$
  *
  * An F test is used to determine whether the null hypothesis should be rejected.
  *
  * This implementation addresses the case for $k$ variables, and is used to determine
  * the F test for the null hypothesis $\mu_1 = \mu_2 = ... = \mu_k$ and common variance $\sigma^2$.
  *
  * In this case we are working with matrices provided by the breeze library, at this time.
  * However, it would be useful to later change the implementation to use another structure
  * which allows uneven lengths of samples, since the number of rows for each sample will be equal in the matrix form.
  *
  * It is possible however to use unqual sample sizes for each $ith$ sample.
  *
  * An [[AnovaTable]] is used to contain the variables for the anova process
  * providing a "one way layout" that comprises of the following elements.
  *
  *
  * The total sum of squares is computed form the $SSE$ and the $SST$
  *
  * $$
  * TotalSS = \sum_{i=1 \in k} \sum_{j=1 \in n_i} (Y_{ij} - \bar{Y})^2
  * $$
  *
  * It can be summarised as being the total of all observations squared subtracting the correction for the mean $CM$
  *
  * $$
  * \sum_{i=1 \in k} \sum_{j=1 \in n_i} Y_{ij}^2 - CM
  * $$
  *
  * where the correction for the mean is calculated as the total for all observations squared divided by $n$
  *
  * $$
  * CM = \frac{1}{n} ( \sum_{i=1 \in k} \sum_{j=1 \in n_i} Y_{ij} )^2
  * $$
  *
  * The total of each sample set is defined as $Y_{i.}$:
  *
  * $$
  * Y_i. = \sum_{j=1 \in n_i} Y_{ij}
  * $$
  *
  * and the mean of each sample set is estimated as $\bar{Y_{i.}}$.
  *
  * $$
  * \bar{Y_{i.} } = \frac{1}{n_i} \sum_{j=1 \in n_i} Y_{ij}
  * $$
  * $$
  * \frac{1}{n_i} Y_{i.}
  * $$
  *
  * This is used in calculating the Sum of squares for treatments which will be large if the
  * differences between the treatments is also large.
  *
  * $$
  * SST = \sum_{i=1 \in k} n_i (\bar{Y_{i.}} - \bar{Y})^2
  * $$
  *
  * Note also that ${Z\ squared} = \frac{SST}{{\sigma^2}}$
  *
  * having a $\chi^2$
  *
  * distribution with $k-1$ df for $k$ factors.
  *
  * $$
  * SST = \sum_{i=1 \in k} \frac{Y_{i.}^2}{n_i} - CM
  * $$
  *
  * The second part the sum of squared errors is computed as
  * $$
  * SSE = Total SS - SST
  * $$
  *
  * However it can also represent a total of the sample variances multiplied by a degree of freedom as shown in Wackerly.
  *
  * $$
  * SSE = \sum_{i=1 \in k} (n_i - 1) S_i^2
  * $$
  *
  * where the sample variance $S^2$ is
  *
  * $$
  * {S&#94;2} = \frac{1}{n_i-1} \sum_{j=1 \in n_i} (Y_{ij} - \bar{Y_{i.}})^2
  * $$
  *
  * which is an unbiased estimator of $ \sigma_i&#94;2 = \sigma&#94;2 $
  *
  *
  *
  * The Mean squared error is an estimator for the pooled variance $S&#94;2$
  * with $n-k$ degrees of freedom.
  *
  * $$
  * MSE = \frac{SSE}{n-k}
  * $$
  *
  * The Mean square total is accumulated from the estimates of the mean for each sample with degree of freedom $k-1$.
  * $$
  * MST = \frac{SST}{k-1}
  * $$
  *
  * Once the anova table is calculated the F-Test is used to test the null hypothesis
  * $\mu_1 = \mu_2 = ... = \mu_k$ with even variance, and is rejected at
  * the critical level $\alpha$
  *
  * $$
  * F = \frac{MST}{MSE} > F_\alpha
  * $$
  * The statistic is an F distribution with $k-1$ and $n-k$ numerator and denominator degrees of freedom.
  *
  * The key assumptions are the normal assumption for the $k$ samples, with equal means and variance.
  *
  * __Exampel Usage__
  *
  * The example is derived from the test case TestAnovaInference which is also derived from an
  * example in Wackerly on page 671.
  *
  * The test data for the example is the following matrix with $k = 4$ sets of observations.
  *
  * {{{
  *   /**
  * columns correspond to k samples
  * rows correspond to sample observation Y_ij
  * */
  * val table = DenseMatrix(
  * (65.0, 75.0, 59.0, 94.0),
  * (87.0, 69.0, 78.0, 89.0),
  * (73.0, 83.0, 67.0, 80.0),
  * (79.0, 81.0, 62.0, 88.0),
  * (81.0, 72.0, 83.0, 0.0),
  * (69.0, 79.0, 76.0, 0.0),
  * (0.0, 90.0, 0.0, 0.0))
  * }}}
  *
  * Note that incomplete examples have been padded with 0.
  * In this implementation it would be best to use a "balanced" set of samples where the number for
  * each observation is equal.
  *
  * The Anova table is created as
  *
  * {{{
  *   val anova = Anova(table)
  * }}}
  *
  *
  * And a test for the null hypothesis at the critical level for $\alpha = 0.05$ to be performed using
  *
  * {{{
  *   val testResult = anova.test(0.05)
  * }}}
  *
  * The test result will contain the anova table which can be printed or inspected for each of the table values.
  * Inspecting the table will give a report for example:
  *
  * {{{
  *      NumeratorDF: 3
  * DenominatorDF: 24
  * SST: 2876.107142857145
  * SSE: 23604.857142857145
  * MSE: 983.5357142857143
  * MST: 958.7023809523816
  * TotalDF: 27
  * TotalSS: 26480.96428571429
  * F-stat (observed statistic): 0.9747509592456765
  * F-alpha (critical value): 3.0000000000000013
  * P-Value: 0.4219003172019309
  * Observed-Prob 0.44613162513336035
  * alpha (significance level):0.05
  *
  * }}}
  *
  * In this example the F-stat > F-alpha and the test case rejects the null hypothesis.
  *
  * The test result also has the "rejected" flag which indicates whether the null hypothesis is rejected.
  * It is possible to use a $k$ fold approach to determine which of the samples may be rejected after the initial test.
  *
  * The critival value is approximated using the trait [[CriticalValue]] for the [[UpperTail]]
  * of the [[FDistribution]].
  *
  *
  * Created by cd on 17/09/2014.
  */
class Anova(val X: DenseMatrix[Double]) extends StatisticalTest {

  /**
    * internal class for intermediate results
    *
    * @param cm
    * @param totalSS
    * @param sst
    * @param sse
    * @param mst
    * @param mse
    */
  class Intermediate(val cm: Double, val totalSS: Double, val sst: Double, val sse: Double, val mst: Double, val mse: Double) {}

  /**
    * f distribution
    * n = rows * cols
    * k = cols
    *
    * df = (k-1), (n-k)
    *
    */
  val n = X.cols * X.rows
  val k = X.cols
  val fdist = FDistribution(k - 1, n - k)

  val criticalVal = CriticalValue(fdist, UpperTail()) _

  /**
    * the correction for the mean
    *
    * @return
    */
  def cm(): Intermediate = {
    val sum = X.toArray.foldLeft(0.0) {
      (n, d) => n + d
    }
    val n = X.rows * X.cols
    val cm = (Math.pow(sum, 2.0) / n.toDouble)
    new Intermediate(cm, 0.0, 0.0, 0.0, 0.0, 0.0)
  }

  /**
    * sum the supplied column in the matrix
    *
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
    *
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
  def totalSS(accum: Intermediate): Intermediate = {
    val CM = accum.cm
    val rows = X.rows
    val cols = X.cols
    val sums = foldColumns(X) { (y: Double) => Math.pow(y, 2.0) }
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
  def ssTreatment(accum: Intermediate): Intermediate = {
    val CM = accum.cm
    val rows = X.rows
    val cols = X.cols
    val sums = foldColumns(X) { (y: Double) => y }
    val ss = sums.map {
      (y: Double) => Math.pow(y, 2.0) / rows
    }.sum
    val sstr = ss - CM
    new Intermediate(CM, accum.totalSS, sstr, 0.0, 0.0, 0.0)
  }

  /**
    * the sum of squares error
    *
    * @return
    */
  def sse(accum: Intermediate): Intermediate = {
    val sse = accum.totalSS - accum.sst
    new Intermediate(accum.cm, accum.totalSS, accum.sst, sse, 0.0, 0.0)
  }

  /**
    * the mean sum of squares
    *
    * @return
    */
  def mse(accum: Intermediate): Intermediate = {
    val k = X.cols
    val n = X.rows * X.cols
    val mse = accum.sse / (n - k)
    new Intermediate(accum.cm, accum.totalSS, accum.sst, accum.sse, 0.0, mse)
  }

  /**
    * the mean sum of squares statistic
    *
    * @return
    */
  def mst(accum: Intermediate): Intermediate = {
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
  def statistic(): (Double, Intermediate) = {
    val k = X.cols
    val n = X.cols * X.rows
    val builder = PartialFunction[Intermediate, Intermediate](_)
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
    *
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
    // get the probability of the observed F value
    val prob = fdist.pdf(stat)
    // calculating the minimum alpha-value for the p Value see Wackerly section 10.6
    // the pvalue in the case of the F-Distribution is equal to P(w_0 >= observedStat)
    // P(w_0 >= W) = 1 - P(w_0 < W)
    val pValue = 1 - fdist.integral(0.0, stat)
    return new AnovaTable(significance = alpha,
      reject = reject,
      pValue = pValue,
      observedProb = prob,
      observedValue = stat,
      criticalValue = test,
      numeratorDf = k - 1,
      denominatorDf = n - k,
      ssTreatment = interim.sst,
      mst = interim.mst,
      sse = interim.sse,
      mse = interim.mse,
      totalDf = n - 1,
      totalSS = interim.totalSS)
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
                   * The p-value for the test the smallest level of significance
                   * for alpha to indicate the null hypothesis should be rejected
                   * therefore the P(X ) < p-value will be rejected.
                   */
                 pValue: Double,

                 /**
                   * the observed probability of the value.
                   */
                 observedProb: Double,

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
      s"""MSE: $mse\n MST: $mst\n""" +
      s"""TotalDF: $totalDf\nTotalSS: $totalSS\n""" +
      s"""F-stat (observed statistic): $observedValue\n""" +
      s"""F-alpha (critical value): $criticalValue\n""" +
      s"""P-Value: $pValue\n""" +
      s"""Observed-Prob $observedProb\n""" +
      s"""alpha (significance level):$significance"""

  }

}
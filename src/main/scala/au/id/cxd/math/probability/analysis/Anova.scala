package au.id.cxd.math.probability.analysis

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
class Anova(X:DenseMatrix[Double]) extends StatisticalTest {

  /**
   * the correction for the mean
   * @return
   */
  def cm():Double = {
    val sum = X.toArray.foldLeft(0.0) {
      (n, d) => n+d
    }
    val n = X.rows * X.cols
    ( 1.0 / n.toDouble ) * Math.pow(sum, 2.0)
  }

  /**
   * sum the supplied column in the matrix
   * @param col
   * @param M
   * @return
   */
  def sumCol (col:Int, M:DenseMatrix[Double])(fn:Double => Double) = {
    val sum = M(::,col).foldLeft(0.0){
      (n,d) => n+fn(d)
    }
    sum
  }

  /**
   * fold an operation accross a matrix and accumulate the output in a vector
   * @param M
   * @param fn
   * @return
   */
  def foldColumns(M:DenseMatrix[Double])(fn:Double => Double):Seq[Double] = {
    val cols = M.cols
    val idx = (0 to (cols-1))
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
  def totalSS() = {
    val CM = cm()
    val rows = X.rows
    val cols = X.cols
    val sums = foldColumns(X) { (y:Double) => Math.pow(y, 2.0) }
    val total = sums.sum
    total - CM
  }

  /**
   * sum of squares treatment
   * $$
   *
   * SST = \sum_{i=1}^k n_i (\bar{Y_{i.} } - \bar{Y} )^2  = \sum_{i=1}^k \frac{Y_{i.}^2}{n_i} - CM
   *
   * $$
   */
  def ssTreatment() = {
    val CM = cm()
    val rows = X.rows
    val cols = X.cols
    val sums = foldColumns(X) { (y:Double) => y }
    val ss = sums.map {
      (y:Double) => Math.pow(y, 2.0) / rows
    }.sum
    ss - CM
  }

  /**
   * the sum of squares error
   * @return
   */
  def sse() = {
    totalSS - ssTreatment
  }

  /**
   * the mean sum of squares
   * @return
   */
  def mse() : Double = {
    val k = X.cols
    val n = X.rows*X.cols
    sse / (n - k)
  }

  /**
   * the mean sum of squares statistic
   * @return
   */
  def mst() : Double = {
    val k = X.cols
    ssTreatment / (k - 1.0)
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
  def statistic() : Double = {
    val k = X.cols
    val n = X.cols*X.rows
    val F = mst / mse
    F
  }

  def test(alpha:Double) : TestResult = {
    val stat = statistic()
    val n = X.cols*X.rows
    val k = X.cols
    // TODO: calculate the critical value F statistic for (k-1), (n-k) df at alpha level
    //
    return TestResult(alpha, false, 0.0, stat, 0.0)
  }

}

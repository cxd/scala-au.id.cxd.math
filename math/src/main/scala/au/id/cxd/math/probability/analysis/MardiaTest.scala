package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.function.distance.Cov
import au.id.cxd.math.probability.continuous.{ChiSquare, Normal}
import breeze.linalg.{DenseMatrix, diag, inv}

/**
  * #import MathJax
  *
  *
  * The main reference for the multivariate tests is the paper discussing the MVN package for R
  *
  * https://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf
  *
  * Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate Normality, The R Journal. 2014 6(2):151-162.
  *
  *
  * Mardia test for multivariate normality based on measures of kurtosis and skewness
  *
  * The matrix R is formed by the covariance variance matrix and the difference between the sample from the column means
  *
  * $$
  * R = (X-\bar{X})'S&#94;{-1}(X-\bar{X})
  * $$
  *
  * The matrix $S$ is the sample estimate of the variance covariance matrix.
  *
  * The measures of kurtosis is fiven as the measure of the ratio between the fourth moment and the third moment of the distribution.
  *
  * And the skewness the ratio between the third and second moments.
  *
  * The measures of skewness $b_{1,p}$ and kurtosis $b_{2,p}$ are calculated as
  *
  * $$
  * b_{1,p} = \frac{1}{n&#94;2} \sum_{i,j=1}&#94;n r&#94;3_{ij}
  * $$
  * and
  * $$
  * b_{2,p} = \frac{1}{n}\sum_{i=1}&#94;n r&#94;2_{ii}
  * $$
  * where $n$ is the number of observations in the sample $X$
  *
  * For large $n$ the test statistic for skewness is derived as
  *
  * $$
  * z_1 = \frac{n}{6}b_{1,p}
  * $$
  * which has a $\chi&#94;2$ distribution with df
  * $$
  * df = p(p+1)(p+2)/6
  * $$
  * in the case of fewer than 20 samples there is a factor $c$ given at
  *
  * and the statistic for kurtosis $z_2$ is
  * $$
  * z_2 = \sqrt{n} \frac{b_{2,p} -  p(p+2) } { \sqrt{8 p(p+2) } }
  * $$
  * which is distributed as $N(0,1)$. It is a standardisation of $b_{2,p}$ with $\mu=p(p+2)$ and $\sigma = \sqrt{ 8p(p+2)/n}$
  *
  * In the case of small sample sizes less then 20 observations, a correction factor $c$ is given for $z_1$
  * $$
  * c = \frac{(n+1)(n+3)(k+1)}{n(n+1)(k+1)-6}
  * $$
  * then $z1 = \frac{nc}{6}b_{1,p}
  *
  *
  * Both tests need to be small in order to not reject the null hypothesis.
  *
  */
class MardiaTest(val alpha:Double) {

  /**
    * estimate the parameters
    * $$
    * \Sigma = C
    * $$
    * the estimate of the variance covariance matrix
    * $$
    * \bar{X}
    * $$
    * the estimate of the column means
    * $$
    * \Delta = X - \bar{X}
    * $$
    * the difference between the mean and the sample.
    * @param X
    */
  private def parameters (X:DenseMatrix[Double]) = {
    val Sigma = Cov(X,X)
    val colMean = ColMeans(X)
    val Mean = DenseMatrix.tabulate(X.rows, X.cols) {
      case (i,j) => colMean(0,j)
    }
    val Delta = X - Mean
    (Delta, Mean, Sigma)
  }

  /**
    * calculate the matrix R
    *
    * note this is a $k \times k$ matrix representing the distance between $ij$ at each point.
    * @param delta
    * @param sigma
    * @return
    */
  private def computeR (delta:DenseMatrix[Double], sigma:DenseMatrix[Double]):DenseMatrix[Double] = {
    val d = delta.t * (inv(sigma) * delta)
    d
  }

  /**
    * calculate the skewness measure for the
    * @param R
    *          is the matrix
    *          $$
    *          (X - \bar{X})' S&#94;{-1} (X - \bar{X})
    *          $$
    */
  private def skewness (n:Double, R:DenseMatrix[Double]) = {
    val total = R.toArray.foldLeft (0.0) {
      (accum, r) => Math.pow(r,3) + accum
    }
    val b1 = (1.0 / Math.pow(n, 2)) * total
    b1
  }

  /**
    * calculate the kurtosis statistic for the supplied matrix
    * @param R
    *          is the matrix
    *          $$
    *          (X - \bar{X})' S&#94;{-1} (X - \bar{X})
    *          $$
    * @return
    */
  private def kurtosis (n:Double, R:DenseMatrix[Double]) = {
    val d = diag(R)
    val total = d.toArray.foldLeft (0.0) {
      (accum, r) => Math.pow(r,2) + accum
    }
    val b2 = (1.0 / n) * total
    b2
  }

  /**
    * perform the mardia test and return the mardia test result.
    * @param X
    * @return
    */
  def mardia(X:DenseMatrix[Double]) = {
    val (delta, mean, sigma) = parameters(X)
    val rMatrix = computeR(delta.t, sigma)
    val n = X.rows
    val p = X.cols
    val b1 = skewness(X.rows, rMatrix)
    val b2 = kurtosis(X.rows, rMatrix)

    /**
      * skewness statistic
      * has chisq distribution of df p(p+1)(p+2)
      */
    val z1 = if (n < 20) {
      // correction for small sample size
      val c =((p+1)*(n+1)*(n+3)) / (n*(n+1)*(p+1) - 6)
      n*c/6.0 * b1
    } else {
      n/6.0 * b1
    }

    /**
      * kurtosis statistic has normal distribution N(0,1)
      */
    val z2 = Math.sqrt(n) * (b2 - p * (p+2)) / Math.sqrt(8.0*p*(p+2))
    val z2mu = 0.0
    val z2Var = 1.0
    /**
    val z2 = b2
    val z2mu = p*(p+2)
    val z2Var = 8*p*(p+2)/n
    **/

    // we should reject the hypothesis if the pvalue is < 0.05 or if the statistic is > critical value
    // H_0: data is multivariate normal
    // H_1: data is not multivariate normal
    val df = p*(p+1)*(p+2)/6.0
    val chisq = ChiSquare(df)
    val pVal_skew = 1.0 - chisq.cdf(z1)
    val cVal_skew = chisq.invcdf(1.0 - alpha)
    val norm = Normal(z2mu)(z2Var)
    val pVal_kurt = 1.0 - norm.cdf(z2)
    val cVal_kurt = norm.invcdf(1.0 - alpha/2.0)
    val stat = MardiaTestStatistic(alpha=alpha,
      skewness = b1,
      skewStat = z1,
      pvalueSkew = pVal_skew,
      critValueSkew = cVal_skew,
      skewDf=df,
      kurtosis = b2,
      kurtosisStat = z2,
      pvalueKurtosis = pVal_kurt,
      critValueKurtosis = cVal_kurt,
      skewTestRejectNull = z1 > cVal_skew,
      kurtotisTestRejectNull = z2 > cVal_kurt
    )
    stat
  }

}

object MardiaTest {
  def apply(alpha:Double, X:DenseMatrix[Double]) =
    new MardiaTest(alpha).mardia(X)
}

/**
  * an object to hold the results output from the mardia test procedure.
  * @param skewness
  * @param skewStat
  * @param pvalueSkew
  * @param critValueSkew
  * @param kurtosis
  * @param kurtosisStat
  * @param pvalueKurtosis
  * @param critValueKurtosis
  * @param skewTestRejectNull
  * @param kurtotisTestRejectNull
  */
case class MardiaTestStatistic (val alpha:Double,
                                val skewness:Double,
                                val skewStat:Double,
                                val pvalueSkew:Double,
                                val critValueSkew:Double,
                                val skewDf:Double,
                                val kurtosis:Double,
                                val kurtosisStat:Double,
                                val pvalueKurtosis:Double,
                                val critValueKurtosis:Double,
                                val skewTestRejectNull:Boolean,
                                val kurtotisTestRejectNull:Boolean) {


  private def result() = {
    if (!skewTestRejectNull && !kurtotisTestRejectNull) {
      "Cannot reject null hypothesis, sample is multivariate normal"
    } else {
      "Reject null hypothesis, sample is not multivariate normal"
    }
  }

  override def toString() =
    s"""
       |Mardia Test at alpha = $alpha
       |Skewness: $skewness
       |Skew Statistic: $skewStat
       |Skew DF: $skewDf
       |Skew P-Value: $pvalueSkew
       |Skew Critical Value: $critValueSkew
       |Skew Test Reject H0:$skewTestRejectNull
       |
       |Kurtosis: $kurtosis
       |Kurtosis Statistic: $kurtosisStat
       |Kurtosis P-Value: $pvalueKurtosis
       |Kurtosis Critical Value: $critValueKurtosis
       |Kurtosis Test Reject H0: $kurtotisTestRejectNull
       |
       |Result: ${result}
     """.stripMargin
}

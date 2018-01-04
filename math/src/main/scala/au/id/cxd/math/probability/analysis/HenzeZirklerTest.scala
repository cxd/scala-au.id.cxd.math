package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.Constants
import au.id.cxd.math.function.distance.{Cov, MahalanobisDistance}
import au.id.cxd.math.probability.continuous.{ChiSquare, LogNormal, Normal}
import breeze.linalg.qr.QR
import breeze.linalg.{DenseMatrix, diag, inv, pinv, qr}
import spire.syntax.cfor

import scala.util.Try

/**
  * #import MathJax
  *
  * The Henze Zirkler test for normality defines the statistic
  *
  * $$
  * HZ = \frac{1}{n}\sum_{i=1}&#94;n\sum_{j=1}&#94;n \exp {-\frac{\beta&#94;2}{2} D_{ij}} - 2(1+\beta&#94;2)&#94;{-p/2} \sum_{i=1}&#94;n \exp{-\frac{\beta&#94;2}{2(1+\beta&#94;2)}D_i} + n(1+2\beta&#94;2)&#94;{-p/2}
  * $$
  * where $p$ is the number of attributes.
  * $$
  * \beta = \frac{1}{\sqrt{2}} (\frac{n(2p+1)}{4} )&#94;{1/(p+4)}
  * $$
  * $$
  * D_{ij} = (x_i - x_j)'S&#94;{-1}(x_i - x_j)
  * $$
  * Is the squared mahalanobis distance between the $ith$ and $jth$ observations.
  * $$
  * D_i = (x_i - \bar{x})'S&#94;{-1}(x_i - \bar{x})
  * $$
  * Is the squared mahalanobis distance between the $ith$ observation and the mean.
  *
  * The test statistic $HZ$ follows a log-normal distribution with $\mu$ and $\sigma&#94;2$
  *
  * $$
  * \mu = 1 - a&#94;{-p/2} \left[1 + \frac{p\beta&#94;2}{a} + \frac{p(p+2)\beta&#94;4}{2a&#94;2}\right]
  * $$
  * $$
  * \sigma&#94;2 = 2(1+4\beta&#94;2)&#94;{-p/2} + 2a&#94;{-p}}\left[ 1+ \frac{2p\beta&#94;4)}{a&#94;2} + \frac{3p(p+2)\beta&#94;8}{4a&#94;4}\right]
  * $$
  * $$
  * - 4w_\beta&#94;{-p/2} \left[1 + \frac{3p\beta&#94;4}{2w_\beta} + \frac{p(p+2)\beta&#94;8}{2w_{\beta}&#94;2}\right]
  * $$
  * with $a = 1 + 2\beta&#94;2$ and $w_\beta = (1+ \beta&#94;2)(1+3\beta&#94;2)$.
  *
  * The log-normalised parameters are given as
  *
  * $$
  * \log(\mu) = \log{\left( \sqrt{ \frac{\mu&#94;4}{\sigma&#94;2 + \mu&#94;2} } \right)}
  * $$
  *
  * $$
  * \log(\sigma&#94;2) = \log{ \left( \frac{\sigma&#94;2 + \mu&#94;2}{\sigma&#94;2} \right)}
  * $$
  *
  * The test statistic is then derived as
  *
  * $$
  * z = \frac{\log(HZ) - \log(\mu)}{\log(\sigma)}
  * $$
  *
  *
  *
  */
class HenzeZirklerTest(val alpha: Double = 0.05) {

  /**
    * generate a symmetric matrix having the distance measure $dist(X_i, X_j)&#94;2$
    *
    * $$
    * Y = X S&#94; X'
    * $$
    * $$
    * y_{ii} = diag(Y')
    * $$
    * $$
    * D_{ij} = 2Y + y_{ii} I + I' y_{ii}
    * $$
    *
    * @param X
    */
  def distanceMatrix_ij(X: DenseMatrix[Double]) = {
    val n = X.rows
    val S = Cov(X).map { k => ((n - 1.0) / n) * k }
    val Sinv = inv(S)
    val Y = X * Sinv * X.t
    val I = DenseMatrix.ones[Double](1, n)
    val y2 = Y.map { y => 2.0 * y }
    val da = diag(Y.t).toDenseMatrix.t * I
    val db = (I.t * diag(Y.t).toDenseMatrix)
    val Dij = y2.t + da + db
    (Dij, S)
  }


  /**
    * generate a symmetric matrix having the distance measure $dist(X_i, X_j)&#94;2$
    *
    * @param X
    */
  def distanceMatrix(X: DenseMatrix[Double]) = {
    // calculate the squared distances
    val distMat = MahalanobisDistance(X).map { d => d * d }
    distMat
  }


  def henzeZirkler(X: DenseMatrix[Double]) = {
    val distMat = distanceMatrix(X)
    val (dist_ij, sMat) = distanceMatrix_ij(X)

    val p = X.cols.toDouble
    val n = X.rows.toDouble

    val beta = 1.0 / Math.sqrt(2.0) * Math.pow((n * (2.0 * p + 1.0)) / 4.0, 1.0 / (p + 4.0))
    val beta2 = beta * beta
    val beta4 = beta2 * beta2
    val beta8 = beta2 * beta2 * beta2 * beta2

    val a = dist_ij.mapPairs {
      (ind, dij) => {
        //if (ind._1 == ind._2) 1.0
        //else {
          val f = -(beta2 / 2.0) * dij
          Math.exp(f)
        //}
      }
    }.toArray.reduce(_ + _)

    val b = distMat.toArray.foldLeft(0.0) {
      (accum, di) =>
        val f = -(beta2 / (2.0 * (1.0 + beta2))) * di
        accum + Math.exp(f)
    }

    // determine the rank of the convariance matrix
    val QR(q, r) = qr(sMat)
    val k = Math.min(sMat.rows, sMat.cols)
    val rank = (for (i <- 0 until k) yield i).foldLeft(0) {
      (accum, i) => {
        val rVal = r(i, i)
        if (Math.abs(rVal) > Constants.DBL_EPSILON) accum + 1
        else accum
      }
    }

    // hz statistic
    //val hz = n*(1.0 / (n*n)) * a - 2.0 * Math.pow(1.0 + beta2, -p / 2.0)*(1d/n) * b + Math.pow(1.0 + 2.0 * beta2, -p / 2.0)
    val hz = if (rank == p)
      n * (1d / (n * n) * a - 2d * Math.pow(1d + beta2, -p / 2d) * (1d / n) * b + Math.pow(1d + 2d * beta2, -p / 2d))
    else n * 4

    val a1 = 1.0 + 2.0 * beta2
    val a2 = a1 * a1
    val a4 = a1 * a1 * a1 * a1
    val a1p2 = Math.pow(a1, -p / 2.0)

    val beta2a = Math.pow(beta, 2.0 / a1)

    val wb = (1.0 + beta2) * (1.0 + 3.0 * beta2)
    val wb2 = wb * wb

    val mu = 1.0 - a1p2 * (1.0 + (p * beta2) / a1 + (p * (p + 2.0) * beta4) / (2.0 * a2))

    val sigma2 = List(
      2.0 * Math.pow(1.0 + 4.0 * beta2, -p / 2.0),
      2.0 * Math.pow(a1, -p) * (1.0 + (2.0 * p * beta4) / a2 + (3.0 * p * (p + 2.0) * beta8) / (4.0 * a4)),
      -4.0 * Math.pow(wb, -p / 2.0) * (1.0 + (3.0 * p * beta4) / (2.0 * wb) + (p * (p + 2.0) * beta8) / (2.0 * wb2))
    ).reduce(_ + _)

    val logHz = Math.log(hz)
    val mu2 = mu * mu
    val mu4 = mu2 * mu2
    val logmu = Math.log(Math.sqrt(mu4 / (sigma2 + mu2)))
    val logsigma = Math.sqrt(Math.log((sigma2 + mu2) / sigma2))

    // the wald statistic
    val z = (logHz - logmu) / logsigma

    // we will perform the wald test against the chisq distribution

    // TODO: determine whether this is the correct parameterisation for the lognormal test.
    // need more data sets to test this.
    val pvalue = 1.0 - LogNormal(logmu, logsigma).cdf(hz)


    // using wald test with p degrees of freedom
    val chisq = ChiSquare(p)
    //val pvalue = chisq.pdf(z)


    //println(s"HZP2:$pvalue2 P1:$pvalue1 P:$pvalue")

    val critValue1 = LogNormal(logmu, logsigma).invcdf(alpha)

    // approximate critical value
    val critValue = chisq.invcdf(alpha)


    val rejecttest = pvalue < alpha
    HenzeZirklerTestResult(
      alpha = this.alpha,
      hzStat = hz,
      zStat = z,
      pValue = pvalue,
      criticalValue = critValue,
      logmu = logmu,
      logsigma2 = logsigma,
      mu = mu,
      sigma2 = sigma2,
      rejectTest = rejecttest
    )
  }
}

object HenzeZirklerTest {
  def apply(alpha: Double, X: DenseMatrix[Double]) = {
    new HenzeZirklerTest(alpha).henzeZirkler(X)
  }
}


case class HenzeZirklerTestResult(
                                   val alpha: Double,
                                   val hzStat: Double,
                                   val zStat: Double,
                                   val pValue: Double,
                                   val criticalValue: Double,
                                   val logmu: Double,
                                   val logsigma2: Double,
                                   val mu: Double,
                                   val sigma2: Double,
                                   val rejectTest: Boolean) {

  def result() = if (rejectTest) "Reject null hypothesis, data is not multivariate normal"
  else "Cannot reject null hypothesis, data is multivariate normal"

  override def toString() = {
    s"""
       |Henze Zirkler Test at alpha = $alpha
       |Henze-Zirkler Statistic: $hzStat
       |Z Wald Statistic: $zStat
       |P-Value: $pValue
       |Critical Value:$criticalValue
       |
       |LogNormal logmu: $logmu
       |LogNormal mu: $mu
       |LogNormal logsigma2: $logsigma2
       |LogNormal sigma2: $sigma2
       |
       |Reject H_0: $rejectTest
       |Result: ${result()}
     """.stripMargin
  }

}

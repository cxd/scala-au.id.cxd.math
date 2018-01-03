package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.distance.MahalanobisDistance
import au.id.cxd.math.probability.continuous.LogNormal
import breeze.linalg.DenseMatrix
import spire.syntax.cfor

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
  * \mu = 1 - \frac{a&#94;{-p/2}(1 + p\beta&#94;{2/a} + (p(p+2)\beta&#94;4))}{2a&#94;2}
  * $$
  * $$
  * \sigma&#94;2 = 2(1+4\beta&#94;2)&#94;{-p/2} + \frac{2a&#94;{-p} (1+2p\beta&#94;4)}{a&#94;2} + \frac{3p(p+2)\beta&#94;8}{4a&#94;4}
  * $$
  * $$
  * - 4w_\beta&#94;{-p/2} (1 + \frac{3p\beta&#94;4}{2w_\beta} + \frac{p(p+2)\beta&#94;8}{2w_{\beta}&#94;2})
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
    * @param X
    */
  def distanceMatrix_test(X: DenseMatrix[Double]) = {
    val rows = X.rows
    val dist = DenseMatrix.zeros[Double](rows, rows)
    // the matrix is symmetric, the distance around the diagonal is 0
    (for (i <- 0 until rows) yield i).foldLeft(dist) {
      (distMat, i) => {
        (for (j <- 0 until rows) yield j).foldLeft(distMat) {
          (distMat1, j) =>
            if (i == j) {
              distMat1.update(i, j, 0.0)
              distMat1
            } else {
              if (distMat1(j, i) != 0.0) {
                distMat1.update(i, j, distMat1(j, i))
              } else if (distMat1(i, j) == 0.0) {
                val x = distMat1(i, ::)
                val y = distMat1(j, ::)
                val dij = MahalanobisDistance(x.inner, y.inner)
                distMat1.update(i, j, dij)
              }
              distMat1
            }
        }
      }
    }
  }


  /**
    * generate a symmetric matrix having the distance measure $dist(X_i, X_j)&#94;2$
    *
    * @param X
    */
  def distanceMatrix(X: DenseMatrix[Double]) = {
    val m = new MahalanobisDistance()
    // calculate the squared distances
    val distMat = m.dist(X).map { d => d * d }
    val distMat_ij = m.distMatrix.map { d => d * d }
    (distMat, distMat_ij)
  }


  def henzeZirkler(X: DenseMatrix[Double]) = {
    val (distMat, dist_ij) = distanceMatrix(X)
    val p = X.cols
    val n = X.rows
    val beta = 1.0 / Math.sqrt(2.0) * Math.pow((n * (2.0 * p + 1.0)) / 4.0, 1.0 / (p + 4.0))
    val beta2 = Math.pow(beta, 2.0)
    val a = dist_ij.mapPairs {
      case (idx, dij) => {
        val f = -beta2 / 2.0 * dij
        Math.exp(f)
      }
    }.toArray.reduce(_ + _)
    val b = distMat.toArray.foldLeft(0.0) {
      (accum, di) => {
        val f = -beta2 / (2.0 * (1 + beta2)) * di
        accum + f
      }
    }
    // hz statistic
    val hz = 1.0 / n * a - 2 * Math.pow(1 + beta2, -p / 2.0) * b + n * Math.pow(1 + 2 * beta2, -p / 2.0)
    // calculate mu, and sigma
    val beta4 = beta2 * beta2
    val beta8 = beta4 * beta4
    val a1 = 1 + 2.0 * beta2
    val beta2a = Math.pow(beta, 2.0 / a1)
    val a2 = a1 * a1
    val a4 = a1 * a1 * a1 * a1
    val a1p2 = Math.pow(a1, -p / 2.0)
    val wb = (1 + beta2) * (1 + 3 * beta2)
    val wb2 = wb * wb
    val mu = 1.0 - (a1p2 * (1 + p * beta2a + (p * (p + 2) * beta4)) / (2 * a2) )
    val sigma2 = List(2 * Math.pow(1 + 4 * beta2, -p / 2.0),
      (2 * Math.pow(a, -p) * (1 + 2 * p * beta4)) / (a * a),
      (3 * p * (p + 2) * beta8) / (4 * a4),
      -4 * Math.pow(wb, -p / 2.0) * (1 + 3 * p * beta4 / 2 * wb + p * (p + 2) * beta8 / (2 * wb2))
    ).reduce(_ + _)

    val logHz = Math.log(hz)
    val mu2 = mu * mu
    val mu4 = mu2 * mu2
    val logmu = Math.log(Math.sqrt(mu4 / (sigma2 + mu2)))
    val logsigma2 = Math.log((sigma2 + mu2) / sigma2)
    val logsigma = Math.log(Math.sqrt((sigma2 + mu2) / sigma2))

    val z = (logHz - logmu) / logsigma

    val pvalue = LogNormal(logmu, logsigma2).pdf(logHz)

    val critValue = LogNormal(logmu, logsigma2).invcdf(alpha)

    val rejecttest = pvalue < alpha
    HenzeZirklerTestResult(
      alpha = this.alpha,
      hzStat = hz,
      zStat = z,
      pValue = pvalue,
      criticalValue = critValue,
      logmu = logmu,
      logsigma2 = logsigma2,
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
                                   val rejectTest: Boolean) {

  def result() = if (rejectTest) "Reject null hypothesis, data is not multivariate normal"
  else "Cannot reject null hypothesis, data is multivariate normal"

  override def toString() = {
    s"""
       |Henze Zirkler Test at alpha = $alpha
       |Henze-Zirkler Statistic: $hzStat
       |Z Wald Statistic: $zStat
       |P-Value: $pValue
       |Critical Value: $criticalValue
       |LogNormal log(mu): $logmu
       |LogNormal log(sigma2): $logsigma2
       |Reject H_0: $rejectTest
       |Result: ${result()}
     """.stripMargin
  }

}

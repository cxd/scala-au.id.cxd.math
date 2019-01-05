package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.moments.{Kurtosis, Skewness}
import au.id.cxd.math.probability.continuous.ChiSquare

/**
  * Test for normality based on test of skewness = 0 and excess kurtosis (k - 3) = 0.
  *
  * https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test
  *
  * The test statistic has a chi-square distribution with df=2.
  *
  * The null hypothesis is that skewness = 0 and excess kurtosis = 0, implying the sample is from a normal distribution.
  *
  * This implementation is a univariate implementation of the test (number of regressors p = 1)
  *
  *
  * The series should be scaled prior to using this test.
  */
class JarqueBeraTest(val series:Seq[Double], val p:Int = 1) extends StatisticalTest {


  def computeStat():Double = {
    val n = series.length
    val k = Kurtosis(series)
    val s = Skewness(series)
    val jb = (n - k + 1)/6.0 * (s*s + 1.0/4.0 * (k - 3)*(k - 3))
    jb
  }

  override def test(alpha: Double): TestResult = {
    val stat = computeStat()
    val chisq = ChiSquare(df=2.0)
    val critVal = chisq.invcdf(alpha)
    val pVal = chisq.pdf(stat)
    val result = TestResult(
      name="Jarque-Bera Test",
      significance = alpha,
      reject = pVal <= alpha,
      pValue = pVal,
      observedValue = stat,
      criticalValue = critVal)
    result
  }
}

object JarqueBeraTest {
  def apply(series:Seq[Double], p:Int = 1) = new JarqueBeraTest(series, p)
}

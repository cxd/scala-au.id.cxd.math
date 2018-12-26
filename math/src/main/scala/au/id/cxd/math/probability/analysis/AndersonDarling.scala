package au.id.cxd.math.probability.analysis

/**
  * AndersonDarling test for univariate distribution
  * https://en.wikipedia.org/wiki/Anderson–Darling_test
  * https://www.itl.nist.gov/div898/handbook/eda/section3/eda35e.htm
  *
  * The implementation is based on the article "Evaluating the Anderson-Darling Distribution" by George Marsaglia
  * The corresponding C implementation used in R for the anderson darling test is given in:
  * https://github.com/bbolker/ADmarsaglia.git
  *
  */
class AndersonDarling(val series: Seq[Double], cdf: Double => Double) extends StatisticalTest {

  /**
    * from line 27 of AnDarl.c from ADmarsaglia library.
    *
    * short computation approximating the Anderson Darling density for P(z)
    *
    * @param z
    */
  private def adinf(z: Double):Double = {
    if (z < 2.0)
      Math.exp(-1.2337141 / z) / Math.sqrt(z) * (2.00012 + (.247105 - (.0649821 - (.0347962 - (.011672 -.00168691 * z) * z) * z) * z) * z)
    else
    /* max |error| < .000002 for z<2, (p=.90816...) */
      Math.exp(-Math.exp(1.0776 - (2.30695 - (.43424 - (.082433 - (.008056 -.0003146 * z) * z) * z) * z) * z))
    /* max |error|<.0000008 for 4<z<infinity */
  }

  /**
    * from line 66 of AnDarl.c from ADmarsaglia library
    *
    * The function AD(n,z) returns Prob(A_n<z) where
    * A_n = -n-(1/n)*[ln(x_1*z_1)+3*ln(x_2*z_2+...+(2*n-1)*ln(x_n*z_n)]
    * z_1=1-x_n, z_2=1-x_(n-1)...z_n=1-x_1, and
    * x_1< x_2 <...< x_n is an ordered set of iid uniform [0,1) variates.
    *
    * @param n
    * @param z
    * @return
    */
  private def ad(n:Int, z:Double):Double = {
    val x = adinf(z)
    val c = .01265 + .1757 / n
    if (x > 0.8) {
      val v = (-130.2137 + (745.2337 - (1705.091 - (1950.646 - (1116.360 - 255.7844 * x) * x) * x) * x) * x) / n
      x + v
    } else if (x < c) {
      val v = x / c
      val v1 = Math.sqrt(v) * (1.0 - v) * (49.0 * v - 102)
      x + v1 * (.0037 / (n * n) + .00078 / n + .00006) / n
    } else {
      val v = (x - c) / (.8 - c)
      val v1 = -.00022633 + (6.54034 - (14.6538 - (14.458 - (8.259 - 1.91864 * v) * v) * v) * v) * v
      x + v1 * (.04213 +.01365 / n) / n
    }
  }


  def computeF(data: Seq[Double]): Seq[Double] = data.map(cdf(_))

  def computeStat(n: Int, forward: Seq[Double], backward: Seq[Double]): (Double,Double) = {
    val indexes: Seq[Int] = for (i <- 1 to n) yield i
    val pairs = indexes.zip(forward.zip(backward))
    val s = pairs.foldLeft(0.0) {
      (accum, pair) => {
        val i = pair._1
        val f = pair._2._1
        val b = pair._2._2
        val c = (2 * i - 1) / n
        val logs = Math.log(f) + Math.log(1.0 - b)
        accum + (c * logs)
      }
    }
    val a = -1.0 * n - s
    // undo the divide by n in computing s to hand back to AD test.
    val z = s*n
    (z, a)
  }

  override def test(alpha: Double): TestResult = {
    val data = series.sorted
    val runA = data
    val runB = data.reverse
    val forward = computeF(runA)
    val backward = computeF(runB)
    val (z,a) =  computeStat(data.length, forward, backward)
    val pval = ad(data.length, z)
    val result = TestResult(significance = alpha,
      reject = pval <= alpha,
      pValue = pval,
      observedValue = a,
      criticalValue = a)
    result
  }
}

object AndersonDarling {
  def apply(series: Seq[Double], cdf: Double => Double) = new AndersonDarling(series, cdf)
}
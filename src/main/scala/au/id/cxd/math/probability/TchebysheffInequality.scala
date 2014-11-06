package au.id.cxd.math.probability

import scala.math._
/**
 * Created by cd on 7/09/2014.
 *
 * apply tchebysheff's inequality
 *
 * used to provide a lower bound estimate for P where
 *
 * P(mu - ksigma < Y < mu + ksigma) >= 1 - 1/k^2
 *
 * this can be used to estimate P when the distribution is unknown.
 *
 */
class TchebysheffInequality(mu:Double, sigma:Double) {

  /**
   * specify the lower and upper bounds for Y
   *
   * eg: P(16 < y < 24) =   1 - 1/k^2      with k = 2 = 1 - 1/4 = 3/4
   *
   * in order to estimate the value of k and estimate P
   * @param lower
   * @param upper
   */
  def pdf(lower:Double, upper:Double) = {
    val k = (upper - mu) / sigma
    val p = if (k == 0.0) {
      0.0
    } else {
      1.0 - 1.0 / pow(k,2.0)
    }
    p
  }

  /**
   * lower tail estimate for probability
   * @param lower
   */
  def pdfLower(lower:Double) = {
    val k = (lower + mu) / sigma
    val p = if (k == 0.0) {
      0.0
    } else {
      1.0 - 1.0 / pow(k,2.0)
    }
    p
  }

  /**
   * upper tail estimate for probability
   * @param upper
   * @return
   */
  def pdfUpper(upper:Double) = {
    val k = (upper - mu) / sigma
    val p = if (k == 0.0) {
      0.0
    } else {
      1.0 - 1.0 / pow(k,2.0)
    }
    p
  }

}

object TchebysheffInequality {
  def apply(mu:Double)(sigma:Double) = new TchebysheffInequality(mu, sigma)
}

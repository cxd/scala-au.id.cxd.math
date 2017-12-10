package au.id.cxd.math.function.beta

import au.id.cxd.math.function.gamma.LogGammaFn
import au.id.cxd.math.probability.continuous.Beta

import scala.annotation.tailrec

class InverseBeta {

  /**
    * bisection method of supplied function
    *
    * The implementation of this bisection method is not used due to the
    * method calling back into the fn parameter.
    *
    * In the GSL the fn parameter is actually the same method of the callee.
    * The termination condition in this case takes a very long time to be met.
    *
    * The bisect method is used for initialisation before performing further approximation,
    *  So the accuracy in this case is being sacrificed for compute time.
    *
    *
    * @param x
    * @param P
    * @param fn
    * @param xtol
    * @param Ptol
    * @return
    */
  /**
  protected def bisect(x: Double, P: Double, fn: (Int, Double) => Double, xtol: Double, Ptol: Double, n:Int, maxN:Int) = {

    @tailrec def inner(x0: Double, x1: Double, xNew: Double, n:Int, maxN:Int): Double = {
      if (n > maxN) {
        xNew
      }
      else if (Math.abs(x1 - x0) <= xtol) {
        xNew
      } else {
        val Px = fn(n+1, xNew)
        if (Math.abs(Px - P) < Ptol) {
          xNew
        } else if (Px < P) {
          val tempX = 0.5 * (xNew + x1)
          inner(xNew, x1, tempX, n+1, maxN)
        } else {
          val tempX = 0.5 * (x0 + xNew)
          inner(x0, xNew, tempX, n+1, maxN)
        }
      }
    }

    inner(0, 1, x, n, maxN)
  }
**/

  /**
    * implementation of GSL gsl_cdf_beta_Pinv betainv.c line: 71
    */
  protected def beta_Pinv(P: Double, a: Double, b: Double, n:Int): Double = {
    if (P < 0.0 || P > 1.0) {
      throw new IllegalArgumentException(s"P is invalid $P")
    }
    if (a < 0.0) {
      throw new IllegalArgumentException(s"A $a is invalid must be > 0")
    }
    if (b < 0.0) {
      throw new IllegalArgumentException(s"B $b is invalid must be > 0")
    }
    if (P == 0.0) {
      0.0
    } else if (P == 1.0) {
      1.0
    } else if (P > 0.5) {
      // gsl_cdf_beta_Qinv
      beta_Qinv(1.0-P,a,b)
    } else {
      val mean = a / (a + b)
      val x1 = if (P < 0.1) {
        val lg_ab = LogGammaFn(a + b)._1
        val lg_a = LogGammaFn(a)._1
        val lg_b = LogGammaFn(b)._1
        val lx = (Math.log(a) + lg_a + lg_b - lg_ab + Math.log(P)) / a
        if (lx <= 0) {
          val temp = Math.exp(lx) * Math.pow(1 - Math.exp(lx), -(b - 1) / 1)
          if (temp > mean) mean
          else temp
        } else mean
      } else mean


      val maxN = 100
      val fn = (n:Int,d:Double) => {

        if (n > maxN) {
          d
        } else beta_Pinv(d, a, b, n+1)
      }
      // Note that the bisection method is used only to obtain an initialisation for x.
      // we skip over this initialisation as in the GSL the implementation there is highly recursive.
      // the implementation in the GSL causes stack overflow due to the high amount of recursion.
      //val x2 = bisect(x1, P, fn, 0.01, 0.01, n, maxN)
      val x2 = x1
      @tailrec def inner(x: Double, dP: Double, phi: Double, n: Int): (Double, Double, Double) = {
        val dP1 = P - Beta(a, b).cdf(x)
        val phi1 = Beta(a, b).pdf(x)
        if (dP1 == 0.0 || n + 1 > maxN) {
          (x, dP1, phi1)
        } else {
          val lambda = dP1 / List(2 * Math.abs(dP1 / x), phi1).max
          val step0 = lambda
          val step1 = -((a - 1) / x - (b - 1) / (1 - x)) * lambda * lambda / 2.0
          val step2 = if (Math.abs(step1) < Math.abs(step0)) {
            step0 + step1
          } else {
            step0 * 2.0 * Math.abs(step0 / step1)
          }
          val xNew = if (x + step2 > 0 && x + step2 < 1) {
            x + step2
          } else {
            Math.sqrt(x) * Math.sqrt(mean)
          }
          if (Math.abs(step0) <= 1e-10 * xNew) {
            (xNew, dP1, phi1)
          } else inner(xNew, dP1, phi1, n + 1)
        }
      }

      val dp = P - Beta(a, b).cdf(x2)
      val phi = Beta(a, b).pdf(x2)
      val (x3, dP1, phi1) = inner(x2, dp, phi, 0)
      x3
    }

  }

  /**
    * implementation of beta_Qinv from GSL betainv.c line 189
    * @param Q
    * @param a
    * @param b
    * @return
    */
  protected def beta_Qinv(Q:Double, a:Double, b:Double):Double = {
    if (Q < 0.0 || Q > 1.0) {
      throw new IllegalArgumentException(s"Q $Q is not in valid range, 0.0 <= Q <= 1.0")
    }
    if (a < 0.0) {
      throw new IllegalArgumentException(s"a $a cannot be < 0")
    }
    if (b < 0.0) {
      throw new IllegalArgumentException(s"b $b cannot be < 0")
    }
    if (Q == 0.0) 1.0
    else if (Q == 1.0) 0.0
    else if (Q > 0.5) beta_Pinv(1.0-Q, a, b, 0)
    else 1 - beta_Pinv(Q,a,b, 0)
  }


  def op(P:Double, a:Double, b:Double) = beta_Pinv(P,a,b, 0)
}

object InverseBeta {
  def apply(P:Double, a:Double, b:Double) = new InverseBeta().op(P,a,b)
}

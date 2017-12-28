package au.id.cxd.math.function.exponential

import au.id.cxd.math.function.approximate.{ContinuedFractionLentz, ContinuedFractionWallis}

/**
  * approximate exponential with continued fraction
  *
  * based on the method exprel_n_CF
  * in exp.c
  */
class ExpRel {

  def op(rN: Double, x: Double) = {

    def isOdd(n: Double): Boolean = {
      if (n % 2.0 == 0.0) false
      else true
    }

    def aFn(n: Double, x: Double):Double = {
      val an = if (n == 0.0) {
        1.0
      } else if (n == 1.0) {
        -x
      } else
      if (isOdd(n)) {
        ((n - 1.0) / 2.0) * x
      } else {
        -(rN + (n / 2) - 1) * x
      }
      an
    }

    def bFn(n: Double, x: Double):Double = {
      val bn = if (n == 0.0) {
        1.0
      } else if (n == 1.0) {
        rN + 1.0
      } else {
        rN + n - 1
      }
      bn
    }

    val maxIter = 5000

    ContinuedFractionWallis(x, aFn, bFn, maxIter)
  }

}

object ExpRel {
  def apply(rN: Double, x: Double) =
    new ExpRel().op(rN, x)
}

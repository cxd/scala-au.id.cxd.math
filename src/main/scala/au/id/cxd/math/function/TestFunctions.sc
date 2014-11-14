import breeze.linalg._
import breeze.numerics.pow

import au.id.cxd.math.probability.regression._
import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function._

import scalaz.Memo

val b1 = Factorial(0.0)
val b2 = Factorial(1.0)
val b3 = Factorial(2.0)

val beta = BetaFn(1.0)(2.0)

/**
 * a memoized factorial operation
 */
def op: (Double => Double) = {
  def innerOp(num:Double) = {
    if (num < 0.0) {
      1.0
    } else {
      num match {
        case 0.0 => 1.0
        case 1.0 => 1.0
        case _ => num * op ( num - 1.0 )
      }
    }
  }
  Memo.mutableHashMapMemo { innerOp }
}

val a = 1.0
val b = 2.0
Math.round(a) - 1.0
Factorial(Math.round(a) - 1.0)
Factorial(Math.round(b) - 1.0)
val test2 = Math.round(a) - Math.round(b) - 1.0

op(test2)
Factorial(Math.round(a) - Math.round(b) - 1.0)
val test = Factorial(Math.round(a) - 1.0) * Factorial(Math.round(b) - 1.0) / Factorial(Math.round(a) - Math.round(b) - 1.0)
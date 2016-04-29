import breeze.linalg._
import breeze.numerics.pow

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

/**
 * > b <- beta(c(1.0,2.0, 3.0), 2.0)
> b
[1] 0.50000000 0.16666667 0.08333333

 */
val alphas = for{ i <- 1 to 3 } yield i
val betas = alphas map { a => BetaFn(a)(2.0) }

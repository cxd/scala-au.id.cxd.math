package au.id.cxd.math.count

import scala.math._
import scalaz.Memo

/**
  * ##import MathJax
  * Created by cd on 6/09/2014.
  *
  * A factorial operation
  */
class Factorial() {

  /**
    * a memoized factorial operation
    */
  def op: (Double => Double) = {
    def innerOp(num: Double) = {
      if (num < 0.0) {
        1.0
      } else {
        num match {
          case 0.0 => 1.0
          case 1.0 => 1.0
          case _ => num * op(num - 1.0)
        }
      }
    }
    Memo.mutableHashMapMemo {
      innerOp
    }
  }

}

object Factorial {

  def apply(num: Double) = {
    new Factorial().op(round(num))
  }
}

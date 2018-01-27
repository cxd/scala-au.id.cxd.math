package au.id.cxd.math.count

import breeze.linalg.DenseMatrix

import scala.math.Ordering

/**
  * given two vectors of two equal lengths
  * create a table with vector a length cols and vector b length rows
  * this table becomes a count matrix where value of row label i is equal to value of col label j
  */
class CrossTabulate[A] {

  def op(a:Seq[A], b:Seq[A])(implicit ord: Ordering[A]) = {


    val uniqueA = a.sorted.distinct
    val uniqueB = b.sorted.distinct

    val merged = (uniqueA ++ uniqueB).sorted.distinct

    val m = DenseMatrix.tabulate[Double](uniqueA.length, uniqueB.length) {
      case (i,j) => {
        val testA = uniqueA(i)
        val testB = uniqueB(j)


        // count the number of times testA index value equals test index value
        val bcount = b.foldLeft((0,0)) {
          (accum, bi) => {
            val ai = a(accum._1)
            bi.equals(testA) && ai.equals(bi) && bi.equals(testB) match {
              case true => (accum._1 + 1, accum._2 + 1)
              case _ =>
                ai.equals(testA) && bi.equals(testB) match {
                  case true => (accum._1 + 1, accum._2 + 1)
                  case _ => (accum._1 + 1, accum._2)
                }

            }
          }
        }
        bcount._2.toDouble
      }
    }

    val m2 = DenseMatrix.tabulate[Double](merged.length, merged.length) {
      case (i,j) => {
        val iIdx = uniqueA.indexOf(merged(i))
        val jIdx = uniqueB.indexOf(merged(j))
        if (iIdx < 0 || jIdx < 0) {
          0.0
        } else {
          m(iIdx,jIdx)
        }
      }
    }

    m2
  }

}
object CrossTabulate {
  def apply[A](a:Seq[A], b:Seq[A])(implicit ord: Ordering[A]) = new CrossTabulate[A]().op(a,b)

  def metrics(m:DenseMatrix[Double]) = {
    val total = m.toArray.sum.toDouble
    val (posTotal, falseTotal) = (for (i<-0 until m.rows) yield i).foldLeft((0.0,0.0)) {
      (accum, i) => (for (j<-0 until m.cols) yield j).foldLeft(accum) {
        (accum1, j) => if (i == j) (accum1._1+m(i,j), accum1._2) else (accum1._1, accum1._2+m(i,j))
      }
    }
    val odds = (posTotal/total)/(falseTotal/total)
    val posPc = (posTotal/total)
    val falsePc = (falseTotal/total)
    (total, posTotal, falseTotal, posPc, falsePc, odds)
  }
}

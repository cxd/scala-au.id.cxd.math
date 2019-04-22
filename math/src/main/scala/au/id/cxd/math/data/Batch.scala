package au.id.cxd.math.data

import scala.annotation.tailrec

/**
  * create sets of indexes for the number of rows specified
  * that partitions the data set into batchSize batches.
  *
  * @param batchSize
  * @param rows
  */
class Batch(batchSize:Int, rows:Int) {

  def skip(data:Seq[Int], n:Int):Seq[Int] = {
    if (data.size >= n) {
      data.slice(n-1, data.size)
    } else Seq.empty[Int]
  }

  @tailrec
  final def breaks(accum:Seq[Seq[Int]], ids:Seq[Int]):Seq[Seq[Int]] = {
    ids.size > 0 match {
      case false => accum
      case true =>
        val section = ids.take(batchSize)
        val accum1 = accum :+ section
        val ids2 = skip(ids, batchSize)
        breaks(accum1, ids2)
    }
  }


  /**
    * return a sequence of sequences representing
    * index positions in rows of a data set.
    * @return
    */
  def op(): Seq[Seq[Int]] = {
    val idx = for (i <- 0 until rows) yield i
    breaks (Seq[Seq[Int]](), idx)
  }


  /**
    * convert the sequence of sequences into
    * index boundaries representing the first and last sequence
    * of partitions in a dataset.
    * @return
    */
  def boundaries():Seq[(Int,Int)] =
    op().map {
      section => (section.head, section.last)
    }


}

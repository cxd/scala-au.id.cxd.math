package au.id.cxd.math.data

import breeze.linalg.DenseMatrix

object Partition {

  /**
    * make a range given a set of rows.
    * @param rows
    * @param percent
    * @return
    */
  def makeRange (rows:Int, percent:Double, offset:Int=0):IndexedSeq[Int] = {
    val total = Math.round(rows * percent).toInt
    for (i <- 0 until total) yield i + offset
  }

  /**
    * generate a 3 tuple of partitions in the data set for train, validate and test partitions.
    * @param data
    * @param train
    * @param valid
    * @param test
    * @return
    */
  def apply(data:Seq[DenseMatrix[Double]], train:Double=0.6, valid:Double=0.2, test:Double=0.2): Seq[(DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double])] = {
    val data1 = data.head
    val trainIdx = makeRange(data1.rows, train)
    val valIdx = makeRange(data1.rows, valid, trainIdx.size )
    val testIdx = makeRange(data1.rows, valid, trainIdx.size + valIdx.size)
    // we assume all data are of the same dimension, we will return a set of 3 tuple for each training set.
    data.map {
      d => {
        val trainD = d(trainIdx,::).toDenseMatrix
        val valD = d(valIdx,::).toDenseMatrix
        val testD = d(testIdx,::).toDenseMatrix
        (trainD, valD, testD)
      }
    }
  }

}

package au.id.cxd.math.data

import au.id.cxd.math.data.filter.Which
import au.id.cxd.math.function.transform.ContinuousTransform
import breeze.linalg.DenseMatrix
/**
  * The data set class contains the full dataset
  * the number of continuous colums and number of discrete columns.
  *
  * The data set contains the continuous data first, followed by the discrete data.
  *
  * Created by cd on 7/05/2016.
  */
class DataSet(val data: DenseMatrix[Double], val continuousCols: Int, val discreteCols: Int, val discreteMapping: Map[String, Set[String]], transform:ContinuousTransform)
  extends Serializable {

  var trainData: DenseMatrix[Double] = DenseMatrix.zeros[Double](1, 1)

  var crossValidateData: DenseMatrix[Double] = DenseMatrix.zeros[Double](1, 1)

  var testData: DenseMatrix[Double] = DenseMatrix.zeros[Double](1, 1)

  /**
    * make a random selection of the data for the supplied number of rows.
    *
    * @param nrows
    * @param data
    * @return
    */
  def makeSet(nrows: Int, data: DenseMatrix[Double]): DenseMatrix[Double] = {
    def randRange(): Seq[Int] = for (i <- 1 to nrows) yield (i * Math.random()).round.toInt
    data(randRange, ::).toDenseMatrix
  }

  /**
    * convert an indicator matrix to a set of class labels
    * based on the internal mappings for the supplied feature name
    * @param featureName
    * @param indicators
    * @return
    */
  def convertToClassLabels(featureName:String, indicators:DenseMatrix[Double]):Seq[String] = {
    val classLabels = discreteMapping.filter(p => p._1.equalsIgnoreCase(featureName)).head._2.toList
    val range = for (i <- 0 until indicators.rows) yield i
    range.foldLeft(Seq[String]()) {
      (accum, i) => {
        val row = indicators(i, ::).inner
        // we now need to find the maximum output value.
        val maxS = indicators.toArray.max
        val idx = Which[Double](row.toArray, d => d == maxS)
        val label = classLabels(idx.head)
        accum :+ label
      }
    }

  }


  /**
    * partition the data into the training set, the cross validation set and the test set.
    */
  def createPartitions(trainPc: Double, cvPc: Double, testPc: Double): Unit = {
    val trainRows = (trainPc * data.rows).round
    val cvRows = (cvPc * data.rows).round
    val testRows = (testPc * data.rows).round

    // first we want to sample without replacement so we'll randomly permute the rows of the matrix.
    val alldata = makeSet(data.rows, data)
    // now we partition the data
    val dataset = Partition(Seq(alldata), trainPc, cvPc, testPc)

    val (train, valid, test) = dataset.head

    trainData = train
    crossValidateData = valid
    testData = test
  }

}


object DataSet {
  def apply(data: DenseMatrix[Double], continuousCols: Int, discreteCols: Int, discreteMapping: Map[String, Set[String]], transform:ContinuousTransform) =
    new DataSet(data, continuousCols, discreteCols, discreteMapping, transform)
}

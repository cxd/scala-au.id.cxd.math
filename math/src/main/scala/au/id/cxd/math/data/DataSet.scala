package au.id.cxd.math.data


import au.id.cxd.math.function.ContinuousTransform
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
  def makeSet(nrows: Int, data: DenseMatrix[Double]) = {
    def randRange() = for (i <- 1 to nrows) yield (i * Math.random()).round.toInt
    data(randRange, ::).toDenseMatrix
  }


  /**
    * partition the data into the training set, the cross validation set and the test set.
    */
  def createPartitions(trainPc: Double, cvPc: Double, testPc: Double): Unit = {
    val trainRows = (trainPc * data.rows).round
    val cvRows = (cvPc * data.rows).round
    val testRows = (testPc * data.rows).round
    trainData = makeSet(trainRows.toInt, data)
    crossValidateData = makeSet(cvRows.toInt, data)
    testData = makeSet(testRows.toInt, data)
  }

}


object DataSet {
  def apply(data: DenseMatrix[Double], continuousCols: Int, discreteCols: Int, discreteMapping: Map[String, Set[String]], transform:ContinuousTransform) =
    new DataSet(data, continuousCols, discreteCols, discreteMapping, transform)
}

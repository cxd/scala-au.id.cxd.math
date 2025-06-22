package au.id.cxd.math.example.network

import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.filter.Which
import au.id.cxd.math.data.{DataSet, DataSetReader, Partition}
import au.id.cxd.math.function.transform.{ContinuousTransform, MinMaxNormalisation}
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import au.id.cxd.math.model.logistic.MultinomialLogisticRegressor
import au.id.cxd.math.model.network.activation.{Identity, Relu, Sigmoid, Softmax}
import au.id.cxd.math.model.network.builder.Sequence
import au.id.cxd.math.model.network.initialisation.{RandomGaussianInitialisation, RandomWeightInitialisation, WeightInitialisationStrategy}
import au.id.cxd.math.model.network.layers.{DenseLayer, InputLayer}
import au.id.cxd.math.model.network.loss.DiscreteCrossEntropy
import au.id.cxd.math.model.network.train.SGDTrainer
import au.id.cxd.math.probability.regression.LogisticLeastSquares
import breeze.linalg.DenseVector

import java.io.File

object ExampleMultinomialLogisticRegression {

  val filename = "data/wine/wine_data.csv"

  val wineDataReader = new DataSetReader {
    /**
      * the continuous columns in the wine data set
      */
    override val continuousCols: List[Int] = (for (i <- 1 to 13) yield i).toList
    override val discreteCols: List[Int] = List(0)
    override val file: File = new File(filename)

    override val continuousFn: ContinuousTransform = MinMaxNormalisation()

  }

  def readData(): DataSet = {

    val dataset = wineDataReader.readSync()
    dataset

  }


  def main(arg: Array[String]) = {
    val data = readData()

    val trainEpochs = 5000

    val classLabels = data.discreteMapping.head._2

    // compare with simple multinomial logistic regressor trained with negative log likelihood
    val multilogit = MultinomialLogisticRegressor(dataSet=data,
      numFeatures = 13,
      numClasses = 3,
      featureColRange= (0, 12),
      targetColRange = (13, 15),
      classLabels = classLabels.toList,
      trainEpochs = trainEpochs)
    val logitmodel = multilogit.train()
    val logitxtab = multilogit.test(logitmodel, classLabels.toList,data)

    println("============================")
    println("")

    println("Multinomial Logistic model performance")
    println(logitxtab._1)

    CrossTabulate.printMetrics(logitxtab._1)
    println("============================")
    println("")

  }

}

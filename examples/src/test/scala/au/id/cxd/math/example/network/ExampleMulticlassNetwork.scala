package au.id.cxd.math.example.network

import java.io.File

import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.{DataSet, DataSetReader}
import au.id.cxd.math.function.transform.{ContinuousTransform, MinMaxNormalisation}
import au.id.cxd.math.model.network.activation.{Identity, Relu, Softmax}
import au.id.cxd.math.model.network.builder.Sequence
import au.id.cxd.math.model.network.initialisation.{RandomGaussianInitialisation, RandomWeightInitialisation, WeightInitialisationStrategy}
import au.id.cxd.math.model.network.layers.{DenseLayer, InputLayer}
import au.id.cxd.math.model.network.loss.{DiscreteCrossEntropy, MeanSquareErrorLoss}
import au.id.cxd.math.model.network.train.SGDTrainer

object ExampleMulticlassNetwork {

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

  def readData():DataSet = {

    val dataset = wineDataReader.readSync()
    dataset

  }


  def buildNetwork(initialisation:WeightInitialisationStrategy):Sequence = {
    Sequence(Seq(
      InputLayer(activation=Identity(), units=13),
      DenseLayer(activation=Relu(), units=15),
      DenseLayer(activation=Relu(), units=9),
      DenseLayer(activation=Relu(), units=3),
      DenseLayer(activation=Softmax(), units=3)
    ), initialisation).compile()
      .asInstanceOf[Sequence]
  }


  def trainNetwork(epochs:Int, trainer:SGDTrainer, network:Sequence): (Seq[Double], Seq[Double], Sequence) = {
    val (loss, valloss, network2) = trainer.train(epochs)(network)

    (loss, valloss, network2)
  }

  def main(arg:Array[String]) = {
    val data = readData()

    val rows = data.trainData.rows
    val cols = 13

    val initialisation = RandomWeightInitialisation(rows, cols)

    val initialisation2 = RandomGaussianInitialisation()

    val network = buildNetwork(initialisation)

    val trainX = data.trainData(::, 0 to 12)
    val trainY = data.trainData(::, 13 to 15)
    val validX = data.crossValidateData(::, 0 to 12)
    val validY = data.crossValidateData(::, 13 to 15)
    val testX = data.testData(::, 0 to 12)
    val testY = data.testData(::, 13 to 15)



    val trainer = SGDTrainer(trainX, trainY, validX, validY,
      learnRate = 0.000001,
      momentum = 0.0000005,
      lossFn = DiscreteCrossEntropy())

    //
    val (loss, valloss, network2) = trainNetwork(2500, trainer, network)

    val classLabels = data.discreteMapping.head._2

    val labels = network2.classify(testX, classLabels.toList)

    val actualLabels = data.convertToClassLabels("origin", testY)

    val xtab = CrossTabulate(actualLabels, labels)
    val metrics1 = CrossTabulate.metrics(xtab)

    println(xtab)

    CrossTabulate.printMetrics(xtab)



  }

}

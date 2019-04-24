package au.id.cxd.math.example.network

import java.io.File

import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.filter.Which
import au.id.cxd.math.data.{DataSet, DataSetReader, Partition}
import au.id.cxd.math.function.transform.{ContinuousTransform, MinMaxNormalisation}
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import au.id.cxd.math.model.network.activation.{Identity, Linear, Relu, Softmax}
import au.id.cxd.math.model.network.builder.Sequence
import au.id.cxd.math.model.network.initialisation.{RandomGaussianInitialisation, RandomWeightInitialisation, WeightInitialisationStrategy}
import au.id.cxd.math.model.network.layers.{DenseLayer, InputLayer}
import au.id.cxd.math.model.network.loss.{DiscreteCrossEntropy, MeanSquareErrorLoss}
import au.id.cxd.math.model.network.train.SGDTrainer
import au.id.cxd.math.probability.regression.LogisticLeastSquares
import breeze.linalg.DenseVector

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

  def readData(): DataSet = {

    val dataset = wineDataReader.readSync()
    dataset

  }


  def buildNetwork(initialisation: WeightInitialisationStrategy): Sequence = {
    Sequence(Seq(
      InputLayer(activation = Identity(), units = 13),
      DenseLayer(activation = Linear(), units = 10),
      //DenseLayer(activation = Relu(), units = 9),
      DenseLayer(activation = Relu(), units = 3),
      DenseLayer(activation = Softmax(), units = 3)
    ), initialisation).compile()
      .asInstanceOf[Sequence]
  }


  def trainNetwork(epochs: Int, trainer: SGDTrainer, network: Sequence): (Seq[Double], Seq[Double], Sequence) = {
    val (loss, valloss, network2) = trainer.train(epochs)(network)

    (loss, valloss, network2)
  }

  def main(arg: Array[String]) = {
    val data = readData()

    val rows = data.trainData.rows
    val cols = 13

    val initialisation = RandomWeightInitialisation()

    val initialisation2 = RandomGaussianInitialisation()

    val network = buildNetwork(initialisation)

    val trainX = data.trainData(::, 0 to 12)
    val trainY = data.trainData(::, 13 to 15)
    val validX = data.crossValidateData(::, 0 to 12)
    val validY = data.crossValidateData(::, 13 to 15)
    val testX = data.testData(::, 0 to 12)
    val testY = data.testData(::, 13 to 15)


    val trainer = SGDTrainer(trainX, trainY, validX, validY,
      learnRate = 0.00001,
      momentum = 0.0000001,
      lossFn = DiscreteCrossEntropy())

    //
    val (loss, valloss, network2) = trainNetwork(15000, trainer, network)

    val classLabels = data.discreteMapping.head._2

    val labels = network2.classify(testX, classLabels.toList)

    val actualLabels = data.convertToClassLabels("origin", testY)

    val xtab = CrossTabulate(actualLabels, labels)
    val metrics1 = CrossTabulate.metrics(xtab)

    println("Network classification performance")
    println(xtab)

    CrossTabulate.printMetrics(xtab)

    // lets compare this with a linear model such as canonical discriminant analysis
    val data1 = data.randData
    val groupNames = data.discreteMapping.head._2
    // we know the data has 3 sites in it 1, 2 and 3 these are mapped below
    val origins = Map("1" -> "Site 1",
      "2" -> "Site 2",
      "3" -> "Site 3")
    // we will create an ordinary partition
    val (train, valid, test) = Partition(Seq(data1), 0.6, 0.2, 0.2).head
    val trainX2 = train(::, 0 to 12)
    val trainY2 = data.convertToClassLabels("origin", train(::, 13 to 15))
    val validX2 = valid(::, 0 to 12)
    val validY2 = data.convertToClassLabels("origin", valid(::, 13 to 15))
    val testX2 = test(::, 0 to 12)
    val testY2 = data.convertToClassLabels("origin", test(::, 13 to 15))

    val (components, coeffs, intercept, percentVar, zMat, cor, groupMeans) =
      CanonicalDiscriminantAnalysis(trainY2.toArray.toList.map(_.toString), trainX2)
    val (testProjection, groupProjection, distances, groupAssignments) =
      CanonicalDiscriminantAnalysis.classify(testX2, coeffs, intercept, groupMeans, groupNames.toList)
    val predictions = groupAssignments.map(_._2)

    val xtab2 = CrossTabulate(testY2, predictions)
    val metrics2 = CrossTabulate.metrics(xtab2)

    println("Canonical discriminant analysis performance")
    println(xtab2)

    CrossTabulate.printMetrics(xtab2)

    // compare with logistic regression labelling.
    // to do this we need 3 models one for each class as we are using a two class logistic regressor.
    val targets1 = Which[String](trainY2, _.equalsIgnoreCase("1"))
    val targets2 = Which[String](trainY2, _.equalsIgnoreCase("2"))
    val targets3 = Which[String](trainY2, _.equalsIgnoreCase("3"))
    val trainY2_target1 = DenseVector.tabulate(trainY2.size) {
      case i => if (targets1.contains(i)) 1.0
      else 0.0
    }
    val trainY2_target2 = DenseVector.tabulate(trainY2.size) {
      case i => if (targets2.contains(i)) 1.0
      else 0.0
    }
    val trainY2_target3 = DenseVector.tabulate(trainY2.size) {
      case i => if (targets3.contains(i)) 1.0
      else 0.0
    }
    val model1 = LogisticLeastSquares(trainX2, trainY2_target1)
    val model2 = LogisticLeastSquares(trainX2, trainY2_target2)
    val model3 = LogisticLeastSquares(trainX2, trainY2_target3)
    Seq(model1, model2, model3).foreach(_.train())

    // to combine them we'll need to use the softmax for the results of each three of the probabilities
    val (y1,c1) = model1.estimate(testX)
    val (y2, c2) = model2.estimate(testX)
    val (y3, c3) = model3.estimate(testX)
    val classes3 = (for (i <- 0 until y1.length) yield i).map {
      i =>
        val (a,b,c) = (y1(i), y2(i), y3(i))
        val total = Seq(a,b,c).map(Math.exp(_)).sum
        val p = Seq(a,b,c).map(x => Math.exp(x)/total)
        val maxP = p.max
        val idx = Which[Double](p, x => x == maxP).head + 1
        idx.toString
    }
    // now we have classes
    val xtab3 = CrossTabulate(testY2, classes3)
    val metrics3 = CrossTabulate.metrics(xtab3)

    println("Logistic Regression performance")
    println(xtab3)

    CrossTabulate.printMetrics(xtab3)


  }

}

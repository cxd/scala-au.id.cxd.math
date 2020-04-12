package au.id.cxd.math.example.network

import java.io.File

import au.id.cxd.math.data.{MatrixReader, Partition}
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.example.probability.regression.ExampleMtCarsBayesRegression.inputFile
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.evaluation.{Efficiency, MAE, MSE, PeakPercentDeviation, RSquared, WillmotsIndex}
import au.id.cxd.math.model.network.activation.{Identity, LeakyRelu, Linear, Relu, Sigmoid}
import au.id.cxd.math.model.network.builder.{Builder, Sequence}
import au.id.cxd.math.model.network.initialisation.{RandomGaussianInitialisation, RandomWeightInitialisation, WeightInitialisationStrategy}
import au.id.cxd.math.model.network.layers.{DenseLayer, InputLayer}
import au.id.cxd.math.model.network.train.SGDTrainer
import au.id.cxd.math.probability.regression.{BayesLinearRegression, OrdLeastSquares}
import breeze.linalg.{DenseMatrix, DenseVector}
import vegas.{Layer, Line, Point, Quantitative, Vegas}
import vegas.spec.Spec.MarkEnums.Bar
import vegas.spec.Spec.TypeEnums.Nominal

object ExampleCarsRegressionNetwork {

  val inputFile = "data/cars/autompg2.csv"

  def readCars(): (StandardisedNormalisation, DenseMatrix[Double], DenseMatrix[Double]) = {
    val reader = new MatrixReader {}
    val M = reader.read(new File(inputFile))
    // normalise M
    val normal = new StandardisedNormalisation()
    val M1 = normal.transform(M)
    val continuous = Seq(2,3,4,5)
    val X = M1(::, continuous)
    val Y = M1(::, 0)
    (normal, X.toDenseMatrix, Y.toDenseMatrix.t)
  }

  def translateY(data:DenseMatrix[Double], norm:StandardisedNormalisation):DenseMatrix[Double] = {
    val mu = norm.meanVector
    val std = norm.sigmaVector
    val ycol = 0
    val mu2 = DenseVector(mu(0))
    val std2 = DenseVector(std(0))
    val norm2 = new StandardisedNormalisation()
    norm2.meanVector = mu2
    norm2.sigmaVector = std2
    norm2.invert(data)
  }

  def partition(X:DenseMatrix[Double], Y:DenseMatrix[Double]):Seq[(DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double])] = {
    val partitions = Partition(Seq(X, Y))
    partitions
  }

  /**
    * create a simple network
    * @return
    */
  def buildNetwork(weightInitialiser:WeightInitialisationStrategy = RandomWeightInitialisation()):Builder = {
    Sequence(Seq(
      InputLayer(activation=Identity(), units=4),
      DenseLayer(activation=Relu(), units=4,
        weightInitialisation=weightInitialiser),
      DenseLayer(activation=Linear(), units=1,
        weightInitialisation=weightInitialiser)
    )).compile()
  }

  /**
    * create a simple network
    * @return
    */
  def buildNetwork2(weightInitialiser:WeightInitialisationStrategy = RandomWeightInitialisation()):Builder = {
    Sequence(Seq(
      InputLayer(activation=Identity(), units=4),
      DenseLayer(activation=Relu(), units=5,
        weightInitialisation=weightInitialiser),
      DenseLayer(activation=Relu(), units=4,
        weightInitialisation=weightInitialiser),
      DenseLayer(activation=Linear(), units=1,
        weightInitialisation=weightInitialiser)
    )).compile()
  }

  def trainNetwork(epochs:Int, trainer:SGDTrainer, network:Sequence): (Seq[Double], Seq[Double], Sequence) = {
    val (loss, valloss, network2) = trainer.train(epochs)(network)

    (loss, valloss, network2)
  }

  def plotTrain(loss:Seq[Double], valloss:Seq[Double], filename:String) = {
    val plotData = loss
      .zip(valloss)
      .zip(1 to loss.size)
      .map {
        tuple => {
          val loss1 = tuple._1._1
          val valloss1 = tuple._1._2
          val epoch = tuple._2
          Map(
            "loss" -> loss1,
            "val_loss" -> valloss1,
            "epoch" -> epoch
          )
        }
      }
    val plot2 =
      Vegas.layered("Observed vs Simulated",
        width=1024.0,
        height=600.0).
        withData(
          plotData
        ).
        withLayers(
          Layer().
            mark(Point).
            encodeX("epoch", Nominal).
            encodeY("loss", Quantitative)
            .encodeColor(value="blue"),
          Layer().
            mark(Line).
            encodeX("epoch", Nominal).
            encodeY("loss", Quantitative)
            .encodeColor(value="blue"),
          Layer().
            mark(Line).
            encodeX("epoch", Nominal).
            encodeY("val_loss", Quantitative)
            .encodeColor(value="red"),
          Layer().
            mark(Point).
            encodeX("epoch", Nominal).
            encodeY("val_loss", Quantitative)
            .encodeColor(value="red"),
          Layer()
        )

    VegasHelper.showPlot(plot2,
      fileName = filename)
  }

  def plot(obs:DenseMatrix[Double], sim:DenseMatrix[Double], filename:String) = {

    val plotData2 = obs(::, 0).toArray
      .zip(sim(::, 0).toArray)
      .zip(1 to obs.rows)
      .map {
        tuple1 => {
          val y1 = tuple1._1._1
          val y2 = tuple1._1._2
          val n = tuple1._2
          Map(
            "obs" -> y1,
            "sim" -> y2,
            "n" -> n
          )
        }
      }


    val plot2 =
      Vegas.layered("Observed vs Simulated",
        width=1024.0,
        height=600.0).
        withData(
          plotData2
        ).
        withLayers(
          Layer().
            mark(Point).
            encodeX("n", Nominal).
            encodeY("obs", Quantitative)
            .encodeColor(value="blue"),
          Layer().
            mark(Line).
            encodeX("n", Nominal).
            encodeY("obs", Quantitative)
            .encodeColor(value="blue"),
          Layer().
            mark(Line).
            encodeX("n", Nominal).
            encodeY("sim", Quantitative)
            .encodeColor(value="red"),
          Layer().
            mark(Point).
            encodeX("n", Nominal).
            encodeY("sim", Quantitative)
            .encodeColor(value="red"),
          Layer()
        )

    VegasHelper.showPlot(plot2,
      fileName = filename)
  }

  def main(args:Array[String]): Unit = {


    val (norm, x, y) = readCars()
    val partitions = partition(x,y)

    val trainX = partitions(0)._1
    val validX = partitions(0)._2
    val testX = partitions(0)._3

    val trainY = partitions(1)._1
    val validY = partitions(1)._2
    val testY = partitions(1)._3

    // momentum=0.000001
    val trainer = SGDTrainer(trainX, trainY, validX, validY, learnRate = 0.000001, momentum=0.000001)

    val rows = x.rows
    val cols = x.cols
    val initialisation = RandomGaussianInitialisation(rows, cols)


    // 437
    // 720
    // 745
    // lr = 0.000001
    val (loss, valloss, network2) = trainNetwork(2200, trainer, buildNetwork(initialisation).asInstanceOf[Sequence])

    val losses = DenseMatrix.horzcat(DenseVector(loss.toArray).toDenseMatrix.t, DenseVector(valloss.toArray).toDenseMatrix.t)

    //val (loss, valloss, network2) = trainNetwork(1500, trainer, buildNetwork2(initialisation).asInstanceOf[Sequence], trainX, trainY, validX, validY)

    plotTrain(loss, valloss, "docs/plots/example_cars_network_train.html")


    val targets = testY(::,0).toDenseMatrix

    val sim = network2.predict(testX)

    val target2 = translateY(targets.t, norm)
    val sim2 = translateY(sim, norm)

    val targetSim = DenseMatrix.horzcat(target2, sim2)

    plot(target2, sim2, "docs/plots/example_cars_network.html")

    val sim3 = network2.predict(trainX)
    val trainTargets = trainY(::,0).toDenseMatrix
    val translateObs2 = translateY(trainTargets.t, norm)
    val translateSim2 = translateY(sim3, norm)

    plot(translateObs2, translateSim2, "docs/plots/example_train_carts_network.html")

    // compare with OLS
    val ols1 = OrdLeastSquares(trainX, trainY.toDenseVector, 1)
    val (est1, error1) = ols1.train()
    // prediction before update
    val Y1 = ols1.predict(testX)
    // store the residuals from the first iteration
    val rSeries1 = List.tabulate(ols1.residuals.length) { i => ols1.residuals(i) }

    val sim4 = translateY(Y1.t, norm)
    plot(target2, sim4, "docs/plots/example_cars_ols.html")

    val targetSim2 = DenseMatrix.horzcat(target2, sim4)


    val metrics1 = Map(
      "RSquared" -> RSquared(targets.toArray.toSeq, sim.toArray.toSeq),
      "Agreement" -> WillmotsIndex(2.0, targets.toArray.toSeq, sim.toArray.toSeq),
      "Efficiency" -> Efficiency(2.0, targets.toArray.toSeq, sim.toArray.toSeq),
      "MAE" -> MAE(target2.toArray.toSeq, sim2.toArray.toSeq),
      "MSE" -> MSE(target2.toArray.toSeq, sim2.toArray.toSeq),
      "PDV" -> PeakPercentDeviation(target2.toArray.toSeq, sim2.toArray.toSeq)
    )

    val metrics2 = Map(
      "RSquared" -> RSquared(targets.toArray.toSeq, Y1.toArray.toSeq),
      "Agreement" -> WillmotsIndex(2.0, targets.toArray.toSeq, Y1.toArray.toSeq),
      "Efficiency" -> Efficiency(2.0, targets.toArray.toSeq, Y1.toArray.toSeq),
      "MAE" -> MAE(target2.toArray.toSeq, sim4.toArray.toSeq),
      "MSE" -> MSE(target2.toArray.toSeq, sim4.toArray.toSeq),
      "PDV" -> PeakPercentDeviation(target2.toArray.toSeq, sim4.toArray.toSeq)
    )

    println("Network model")
    printMetrics(metrics1)

    println("Bayes OLS model")
    printMetrics(metrics2)
  }


  def printMetrics(metrics:Map[String,Double]) = {
    metrics.foreach {
      pair => {
        println(s"${pair._1} : ${pair._2}")
      }
    }
  }

}

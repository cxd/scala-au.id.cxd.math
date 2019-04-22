package au.id.cxd.math.example.network

import java.io.File

import au.id.cxd.math.data.{MatrixReader, Partition}
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.example.probability.regression.ExampleMtCarsBayesRegression.inputFile
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.network.activation.{Identity, Linear, Relu}
import au.id.cxd.math.model.network.builder.{Builder, Sequence}
import au.id.cxd.math.model.network.layers.{DenseLayer, InputLayer}
import au.id.cxd.math.model.network.train.SGDTrainer
import breeze.linalg.{DenseMatrix, DenseVector}
import vegas.{Layer, Line, Point, Quantitative, Vegas}
import vegas.spec.Spec.MarkEnums.Bar
import vegas.spec.Spec.TypeEnums.Nominal

object ExampleCarsNetwork {

  val inputFile = "data/cars/mtcars.csv"

  def readCars() = {
    val reader = new MatrixReader {}
    val M = reader.read(new File(inputFile))
    // normalise M
    val normal = new StandardisedNormalisation()
    val M1 = normal.transform(M)
    val continuous = Seq(1,2,3,4,5,6,7,9,10)
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
  def buildNetwork():Builder = {
    Sequence(Seq(
      InputLayer(activation=Identity(), units=9),
      DenseLayer(activation=Relu(), units=5),
      DenseLayer(activation=Linear(), units=1)
    )).compile()
  }

  def trainNetwork(epochs:Int, trainer:SGDTrainer, network:Sequence, trainX:DenseMatrix[Double], trainY:DenseMatrix[Double], validX:DenseMatrix[Double], validY:DenseMatrix[Double]): (Seq[Double], Seq[Double], Sequence) = {
    val (loss, valloss, network2) = trainer.train(epochs)(network)

    (loss, valloss, network2)
  }

  def plot(obs:DenseMatrix[Double], sim:DenseMatrix[Double]) = {

    val plotData2 = obs(::, 0).data
      .zip(sim(::, 0).data)
      .zip(1 to obs.cols)
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
      fileName = "docs/plots/example_cars_network.html")
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

    val trainer = SGDTrainer(trainX, trainY, validX, validY, learnRate = 0.0005, momentum=0.001)

    val (loss, valloss, network2) = trainNetwork(21, trainer, buildNetwork().asInstanceOf[Sequence], trainX, trainY, validX, validY)

    val targets = testY(::,0).toDenseMatrix

    val testDataX = trainer.addBias(testX)

    val sim = network2.transfer(testDataX)

    val target2 = translateY(targets.t, norm)
    val sim2 = translateY(sim.t, norm)

    plot(target2.t, sim2.t)

  }

}

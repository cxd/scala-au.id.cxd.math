package au.id.cxd.math.examples.probability.regression

import java.awt.GridLayout
import java.io.File
import javax.swing.JFrame

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.probability.regression.OrdLeastSquares
import breeze.linalg.DenseVector
import org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scalax.chart.module.ChartFactories.XYLineChart
import scala.collection.JavaConversions._

/**
  * An example multivariate regression over the feul consumption for a set of cars
  * given a set of attributes from the R dataset mtcars
  *
  * <pre>
  * mtcars - A data frame with 32 observations on 11 variables.
  * *
  * [, 1]	mpg	Miles/(US) gallon
  * [, 2]	cyl	Number of cylinders
  * [, 3]	disp	Displacement (cu.in.)
  * [, 4]	hp	Gross horsepower
  * [, 5]	drat	Rear axle ratio
  * [, 6]	wt	Weight (1000 lbs)
  * [, 7]	qsec	1/4 mile time
  * [, 8]	vs	V/S
  * [, 9]	am	Transmission (0 = automatic, 1 = manual)
  * [,10]	gear	Number of forward gears
  * [,11]	carb	Number of carburetors
  * </pre>
  *
  * Created by cd on 1/05/2016.
  */
object ExampleMtCarsConsumption {

  val inputFile = "data/cars/mtcars.csv"

  def readCars() = {
    val reader = new MatrixReader {}
    val M = reader.read(new File(inputFile))
    val X = M(::, 1 until M.cols)
    val Y = M(::, 0)
    (X, Y.toDenseVector)
  }

  def main(args: Array[String]) = {
    val (xMat, yVec) = readCars
    val ols1 = OrdLeastSquares(xMat, yVec, 1)
    val (est1, error1) = ols1.train()
    val Y1 = ols1.predict(xMat)

    val ols2 = OrdLeastSquares(xMat, yVec, 2)
    val (est2, error2) = ols2.train()
    val Y2 = ols2.predict(xMat)

    val ols3 = OrdLeastSquares(xMat, yVec, 3)
    val (est3, error3) = ols3.train()
    val Y3 = ols3.predict(xMat)

    val series = new XYSeriesCollection()
    val index = for (i <- 0 until yVec.length) yield i
    val seriesA = new XYSeries("original")
    // just plot the original
    index foreach { i => seriesA.add(i, yVec(i))}

    val series1 = new XYSeries("predict df=1")
    index foreach { i => series1.add(i, Y1(0,i))}

    val series2 = new XYSeries("predict df=2")
    index foreach { i => series2.add(i, Y2(0,i)) }


    val series3 = new XYSeries("predict df=3")
    index foreach { i => series3.add(i, Y3(0,i)) }


    series.addSeries(seriesA)
    series.addSeries(series1)
    series.addSeries(series2)
    series.addSeries(series3)

    val chart = XYLineChart(series)
    chart.title = "mtcars data set and predicted miles per gallon"

    val plot1 = chart.peer

    val dataset = new DefaultBoxAndWhiskerCategoryDataset()

    val rSeries1 = List.tabulate(ols1.residuals.length) { i => ols1.residuals(i) }
    dataset.add(rSeries1, "df-1", "df-1")

    val rSeries2 = List.tabulate(ols2.residuals.length) { i => ols2.residuals(i) }
    dataset.add(rSeries2, "df-2", "df-2")


    val rSeries3 = List.tabulate(ols2.residuals.length) { i => ols3.residuals(i) }
    dataset.add(rSeries3, "df-3", "df-3")


    val box = scalax.chart.module.BoxAndWhiskerChartFactories.BoxAndWhiskerChart(dataset, title="Residuals of 3 models")


    val frame = new JFrame("Example mtcars data set and predicted miles per gallon")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1,2))


    frame.add(ChartHelper.makeChartPanel(plot1, 400, 300))
    frame.add(ChartHelper.makeChartPanel(box.peer, 400, 300))

    frame.pack()
    frame.setVisible(true)


    println(s"Model 3: P-Values\n")
    printValues(ols3.betaPValue.toDenseVector)
    println("\n")
    println("Model 3: Z-Values\n")
    printValues(ols3.betaZScore.toDenseVector)
  }

  def printValues(vals:DenseVector[Double]) = {
    vals.toArray foreach {
      v => println(v)
    }
  }
}


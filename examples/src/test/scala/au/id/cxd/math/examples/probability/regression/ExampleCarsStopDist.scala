package au.id.cxd.math.examples.probability.regression

import java.awt.GridLayout
import java.io.File
import javax.swing.JFrame

import au.id.cxd.math.data.{CsvReader, MatrixReader}
import au.id.cxd.math.probability.regression.OrdLeastSquares
import breeze.linalg.DenseMatrix
import org.jfree.chart.ChartPanel
import org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scalax.chart.module.ChartFactories.XYLineChart
import scalax.chart.module.BoxAndWhiskerChartFactories._
import scala.collection.JavaConversions._

/**
  * Another example of univariate regression using the cars data from the R data sets.
  *
  * A regression model is generated for the equation dist ~ B*speed
  *
  * This program provides an example of generating 3 models plotting the residuals of each.
  *
  *
  *
  * Created by cd on 30/04/2016.
  */
object ExampleCarsStopDist {

  val inputFile = "data/cars/cars.csv"

  /**
    * read the cars file
    */
  def readCars() = {
    val reader = new MatrixReader {}
    val M = reader.read(new File(inputFile))
    val X = M(::,0)
    val Y = M(::,1)
    (X.toDenseMatrix.t,Y.toDenseVector)
  }



  def main(args:Array[String]):Unit = {
    val (xMat, yMat) = readCars()
    val X = xMat
    val ols = OrdLeastSquares(X, yMat, 1)
    val ols2 = OrdLeastSquares(X, yMat, 2)
    val ols3 = OrdLeastSquares(X, yMat, 3)
    println(s"Train df=1")
    val (estimate, error) = ols.train()
    println(s"MSE: $error")
    println(s"Train df=2")
    val (estimate2, error2) = ols2.train()
    println(s"MSE: $error2")
    println(s"Train df=3")
    val (estimate3, error3) = ols3.train()
    println(s"MSE: $error3")

    val Y1 = ols.predict(xMat)
    val Y2 = ols2.predict(xMat)
    val Y3 = ols3.predict(xMat)

    val series = new XYSeriesCollection()
    val index = for (i <- 0 until yMat.length) yield i
    val seriesA = new XYSeries("original")
    index foreach { i => seriesA.add(X(i,0), yMat(i))}

    val series1 = new XYSeries("predict df = 1")
    index foreach { i => series1.add(X(i,0), Y1(0,i))}

    val series2 = new XYSeries("predict df = 2")
    index foreach { i => series2.add(X(i,0), Y2(0,i))}


    val series3 = new XYSeries("predict df = 3")
    index foreach { i => series3.add(X(i,0), Y3(0,i))}


    series.addSeries(seriesA)
    series.addSeries(series1)
    series.addSeries(series2)
    series.addSeries(series3)


    val chart = XYLineChart(series)
    chart.title = "Example car speed and predicted stopping distance"

    val plot1 = chart.peer


    val residuals3 = ols3.residuals

    val dataset = new DefaultBoxAndWhiskerCategoryDataset()

    val rSeries1 = List.tabulate(ols.residuals.length) { i => ols.residuals(i) }
    val rSeries2 = List.tabulate(ols2.residuals.length) { i => ols2.residuals(i) }
    val rSeries3 = List.tabulate(residuals3.length) { i => residuals3(i) }
    dataset.add(rSeries1, "df-1", "df-1")
    dataset.add(rSeries3, "df-2", "df-2")
    dataset.add(rSeries3, "df-3", "df-3")
    val box = scalax.chart.module.BoxAndWhiskerChartFactories.BoxAndWhiskerChart(dataset, title="Residuals of 3 models")


    val frame = new JFrame("Example car speed and predicted stopping distance")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1,2))


    frame.add(ChartHelper.makeChartPanel(plot1, 400, 300))
    frame.add(ChartHelper.makeChartPanel(box.peer, 400, 300))

    frame.pack()
    frame.setVisible(true)
  }


}

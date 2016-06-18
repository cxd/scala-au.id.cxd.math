package au.id.cxd.math.examples.probability.regression

import java.awt.GridLayout
import java.io.File
import javax.swing.JFrame

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.StandardisedNormalisation
import au.id.cxd.math.probability.regression.{BayesLinearRegression, OrdLeastSquares}
import org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scalax.chart.module.ChartFactories.XYLineChart
import scala.collection.JavaConversions._

/**
  * Created by cd on 31/05/2016.
  */
object ExampleMtCarsBayesRegression {

  val inputFile = "data/cars/mtcars.csv"

  def readCars() = {
    val reader = new MatrixReader {}
    val M = reader.read(new File(inputFile))
    // normalise M
    val normal = new StandardisedNormalisation()
    val M1 = new StandardisedNormalisation().transform(M)
    val continuous = Seq(1,2,3,4,5,6,7,9,10)
    val X = M1(::, continuous)
    val Y = M1(::, 0)
    (normal, X.toDenseMatrix, Y.toDenseVector)
  }

  def main(args: Array[String]) = {
    val (normal, xMat, yVec) = readCars
    val ols1 = BayesLinearRegression(xMat, yVec, 1)
    val (est1, error1) = ols1.train()
    // prediction before update
    val Y1 = ols1.predict(xMat)
    // store the residuals from the first iteration
    val rSeries1 = List.tabulate(ols1.residuals.length) { i => ols1.residuals(i) }

    // test update function
    val (est2, error2) = ols1.update(xMat, yVec)
    // store the residuals from the second iteration
    val rSeries2 = List.tabulate(ols1.residuals.length) { i => ols1.residuals(i) }

    val Y2 = ols1.predict(xMat)

    val series = new XYSeriesCollection()
    val index = for (i <- 0 until yVec.length) yield i
    val seriesA = new XYSeries("original")
    // just plot the original
    index foreach { i => seriesA.add(i, yVec(i)) }

    val series1 = new XYSeries("predict df=1")
    index foreach { i => series1.add(i, Y1(0, i)) }

    val series2 = new XYSeries("predict updated df=1")
    index foreach { i => series2.add(i, Y2(0,i)) }

    series.addSeries(seriesA)
    series.addSeries(series1)
    series.addSeries(series2)

    val chart = XYLineChart(series)
    chart.title = "mtcars data set and predicted miles per gallon\n(standardised)"

    val plot1 = chart.peer

    val dataset = new DefaultBoxAndWhiskerCategoryDataset()

    dataset.add(rSeries1, "df-1", "df-1")
    dataset.add(rSeries2, "df-updated", "df-updated")


    val box = scalax.chart.module.BoxAndWhiskerChartFactories.BoxAndWhiskerChart(dataset, title = "Residuals of 2 models")


    val frame = new JFrame("Example mtcars data set and predicted miles per gallon")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1, 2))


    frame.add(ChartHelper.makeChartPanel(plot1, 400, 300))
    frame.add(ChartHelper.makeChartPanel(box.peer, 400, 300))

    frame.pack()
    frame.setVisible(true)

  }

}

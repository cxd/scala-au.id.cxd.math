package au.id.cxd.math.example.probability.regression

import java.awt.GridLayout
import java.io.File
import javax.swing.JFrame

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.ChartHelper
import au.id.cxd.math.probability.regression.LogisticLeastSquares
import breeze.linalg.{DenseMatrix, DenseVector}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.util.ShapeUtilities

import scalax.chart.module.ChartFactories.XYLineChart

/**
  * An example of logistic regression using the mtcars dataset
  *
  * Instead of predicting the miles per gallon, this example will predict the class of the vehicle
  * whether it is automatic or manual.
  *
  * It will use the horsepower and weight values to make this prediction.
  *
  * This follows the same example as that described in:
  *
  * http://www.r-tutor.com/elementary-statistics/logistic-regression/estimated-logistic-regression-equation
  *
  * The dataset mtcars has the properties
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
  * In this example we will use the wt and hp columns to predict the am column.
  *
  *
  * Created by cd on 1/05/2016.
  */
object ExampleCarsLogitRegression {

  val inputFile = "data/cars/mtcars.csv"

  def readCars() = {
    val reader = new MatrixReader {}
    val M = reader.read(new File(inputFile))
    val X = DenseMatrix.tabulate[Double](M.rows, 2) {
      case (i, j) => j match {
        case 0 => M(i, 3)
        case 1 => M(i, 5)
      }
    }
    val Y = M(::, 8)
    (X, Y.toDenseVector)
  }


  def main(args: Array[String]) = {
    val (xMat, yVec) = readCars()
    val ols1 = LogisticLeastSquares(xMat, yVec, 1)
    val ols2 = LogisticLeastSquares(xMat, yVec, 2)
    val ols3 = LogisticLeastSquares(xMat, yVec, 3)
    val (est1, error1) = ols1.train()
    val (est2, error2) = ols2.train()
    val (est3, error3) = ols3.train()
    val (y1, classY1) = ols1.estimate(xMat)
    val (y2, classY2) = ols2.estimate(xMat)
    val (y3, classY3) = ols3.estimate(xMat)

    // draw a scatter plot of the data hp x weight

    val seriesA = new XYSeriesCollection()
    val index = for (i <- 0 until yVec.length) yield i

    val series1 = new XYSeries("class=0")
    index foreach { i => {
      val cls = yVec(i)
      cls match {
        case 0 => series1.add(xMat(i, 0), xMat(i, 1))
        case _ => ()
      }
    }
    }

    val series2 = new XYSeries("class=1")
    index foreach { i => {
      val cls = yVec(i)
      cls match {
        case 1 => series2.add(xMat(i, 0), xMat(i, 1))
        case _ => ()
      }
    }
    }

    seriesA.addSeries(series1)
    seriesA.addSeries(series2)

    val seriesB = new XYSeriesCollection()
    val line1A = new XYSeries("class=0, df=1")
    index foreach { i => classY1(i, 2) match {
      case 0 => {
        line1A.add(xMat(i, 0), xMat(i, 1))
      }
      case 1 => ()
    }
    }
    val line1B = new XYSeries("class=1, df=1")
    index foreach { i => classY1(i, 2) match {
      case 1 => {
        line1B.add(xMat(i, 0), xMat(i, 1))
      }
      case 0 => ()
    }
    }
    seriesB.addSeries(line1A)
    seriesB.addSeries(line1B)


    val seriesC = new XYSeriesCollection()

    val line2A = new XYSeries("class=0,df=2")
    index foreach { i => classY2(i, 2) match {
      case 0 => {
        line2A.add(xMat(i, 0), xMat(i, 1))
      }
      case 1 => ()
    }
    }
    val line2B = new XYSeries("class=1,df=2")
    index foreach { i => classY2(i, 2) match {
      case 1 => {
        line2B.add(xMat(i, 0), xMat(i, 1))
      }
      case 0 => ()
    }
    }
    seriesC.addSeries(line2A)
    seriesC.addSeries(line2B)

    val seriesD = new XYSeriesCollection()
    val line3A = new XYSeries("class=0, df=3")
    index foreach { i => classY3(i, 2) match {
      case 0 => {
        line3A.add(xMat(i, 0), xMat(i, 1))
      }
      case 1 => ()
    }
    }
    val line3B = new XYSeries("class=1,df=3")
    index foreach { i => classY3(i, 2) match {
      case 1 => {
        line3B.add(xMat(i, 0), xMat(i, 1))
      }
      case 0 => ()
    }
    }
    seriesD.addSeries(line3A)
    seriesD.addSeries(line3B)



    //series.addSeries(line2)
    //series.addSeries(line3)

    val chart = XYLineChart.shapes(seriesA)
    val plot = chart.plot

    val chart2 = XYLineChart.shapes(seriesB)
    val renderer = chart2.plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]

    val cross1 = ShapeUtilities.createDiagonalCross(3, 1);
    val cross2 = ShapeUtilities.createRegularCross(3, 1)

    renderer.setSeriesShapesFilled(0,false)
    renderer.setSeriesShapesFilled(1, false)


    //renderer.setSeriesShapesFilled(2,false)
    //renderer.setSeriesShapesFilled(3, false)
    renderer.setSeriesShape(0, cross1)
    renderer.setSeriesShape(1, cross2)


    val chart3 = XYLineChart.shapes(seriesC)
    val renderer3 = chart3.plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
    renderer3.setSeriesShape(0, cross1)
    renderer3.setSeriesShape(1, cross2)

    val chart4 = XYLineChart.shapes(seriesD)
    val renderer4 = chart3.plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
    renderer4.setSeriesShape(0, cross1)
    renderer4.setSeriesShape(1, cross2)

    val frame = new JFrame("Example mtcars hp x weight 0=auto, 1=manual")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1,2))


    frame.add(ChartHelper.makeChartPanel(chart.peer, 200, 200))
    frame.add(ChartHelper.makeChartPanel(chart2.peer, 200, 200))
    frame.add(ChartHelper.makeChartPanel(chart3.peer, 200, 200))
    frame.add(ChartHelper.makeChartPanel(chart4.peer, 200, 200))

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

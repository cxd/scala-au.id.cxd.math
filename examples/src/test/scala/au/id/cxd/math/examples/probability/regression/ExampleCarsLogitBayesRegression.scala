package au.id.cxd.math.examples.probability.regression

import java.awt.GridLayout
import java.io.File
import javax.swing.JFrame

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.probability.regression.{BayesLogisticLeastSquares, LogisticLeastSquares}
import breeze.linalg.DenseMatrix
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.util.ShapeUtilities

import scalax.chart.module.ChartFactories.XYLineChart

/**
  * Test logistic regression with an update
  * Created by cd on 18/06/2016.
  */
object ExampleCarsLogitBayesRegression {
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


  def main(args:Array[String]): Unit = {
    val (xMat, yVec) = readCars()
    val ols1 = BayesLogisticLeastSquares(xMat, yVec, 1)
    val (est1, error1) = ols1.train()
    val (y1, classY1) = ols1.estimate(xMat)
    val (est2, error2) = ols1.update(xMat, yVec)
    val (y2, classY2) = ols1.estimate(xMat)

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

    val line2A = new XYSeries("class=0,updated")
    index foreach { i => classY2(i, 2) match {
      case 0 => {
        line2A.add(xMat(i, 0), xMat(i, 1))
      }
      case 1 => ()
    }
    }
    val line2B = new XYSeries("class=1,updated")
    index foreach { i => classY2(i, 2) match {
      case 1 => {
        line2B.add(xMat(i, 0), xMat(i, 1))
      }
      case 0 => ()
    }
    }
    seriesC.addSeries(line2A)
    seriesC.addSeries(line2B)

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

    val frame = new JFrame("Example mtcars hp x weight 0=auto, 1=manual")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1,2))


    frame.add(ChartHelper.makeChartPanel(chart.peer, 200, 200))
    frame.add(ChartHelper.makeChartPanel(chart2.peer, 200, 200))
    frame.add(ChartHelper.makeChartPanel(chart3.peer, 200, 200))

    frame.pack()
    frame.setVisible(true)

  }
}

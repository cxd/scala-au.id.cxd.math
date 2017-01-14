package au.id.cxd.math.example.probability.regression

import au.id.cxd.math.probability.regression.OrdLeastSquares
import breeze.linalg.{DenseMatrix, DenseVector}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scalax.chart.module.ChartFactories.XYLineChart

/**
  * An example sin wav approximation
  * Created by cd on 29/04/2016.
  */
object ExampleSinWave {

  lazy val input = {
    for (i <- 0.0 to Math.PI*2.0 by 0.1) yield i
  }



  def main(args:Array[String]):Unit = {
    val X = DenseMatrix.tabulate[Double](input.length, 1) {
      case (i, j) => input(i)
    }
    val Y = DenseVector.tabulate[Double](input.length) {
      i => Math.sin(input(i))
    }

    val ols1 = OrdLeastSquares(X, Y, 1)
    val ols2 = OrdLeastSquares(X, Y, 2)

    val ols3 = OrdLeastSquares(X, Y, 3)
    println("Train df=1")
    val (t1, sqError1) = ols1.train()
    println(s"df=1 MSE=$sqError1")
    println("train df=2")
    val (t2, sqError2) = ols2.train()
    println(s"df=2 MSE=$sqError2")
    println("train df=2")
    val (t, sqError) = ols3.train()
    println(s"df=3 MSE=$sqError")

    val predictX = X.toDenseVector

    val Y1 = ols1.predictSeq(predictX) .toDenseVector
    val Y2 = ols2.predictSeq(predictX) .toDenseVector

    val Y3 = ols3.predictSeq(predictX) .toDenseVector


    val series = new XYSeriesCollection()
    val index = for (i <- 0 until Y.length) yield i
    val seriesA = new XYSeries("original")
    index foreach { i => seriesA.add(X(i,0), Y(i))}

    val series1 = new XYSeries("prediction df=1")
    index foreach { i => series1.add(X(i,0), Y1(i))}


    val series2 = new XYSeries("prediction df=2")
    index foreach { i => series2.add(X(i,0), Y2(i))}


    val series3 = new XYSeries("prediction df=3")
    index foreach { i => series3.add(X(i,0), Y3(i))}


    series.addSeries(seriesA)

    series.addSeries(series1)
    series.addSeries(series2)
    series.addSeries(series3)

    val chart = XYLineChart(series)
    chart.title = "Example sin wave approximation"
    chart.show()

  }

}

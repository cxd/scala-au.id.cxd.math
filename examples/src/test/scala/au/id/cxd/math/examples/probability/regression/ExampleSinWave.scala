package au.id.cxd.math.examples.probability.regression

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
    val X = DenseMatrix.tabulate[Double](1, input.length) {
      case (i, j) => input(j)
    }
    val Y = DenseVector.tabulate[Double](input.length) {
      i => Math.sin(input(i))
    }

    val ols1 = OrdLeastSquares(X, Y, 1, 0.5)
    val ols2 = OrdLeastSquares(X, Y, 2, 0.5)

    val ols3 = OrdLeastSquares(X, Y, 3, 0.5)
    val (t, sqError) = ols3.train()
    val (t1, sqError1) = ols1.train()
    val (t2, sqError2) = ols2.train()

    val predictX = DenseVector.tabulate[Double](input.length) { j => X(0,j) }

    val Y1:DenseMatrix[Double] = ols1.predictSeq(predictX)
    val Y2 = ols2.predictSeq(predictX)

    val Y3 = ols3.predictSeq(predictX)


    val series = new XYSeriesCollection()
    val index = for (i <- 0 until Y.length) yield i
    val seriesA = new XYSeries("original")
    index foreach { i => seriesA.add(X(0,i), Y(i))}

    val series1 = new XYSeries("prediction df=1")
    index foreach { i => series1.add(X(0,i), Y1(i,0))}


    val series2 = new XYSeries("prediction df=2")
    index foreach { i => series2.add(X(0,i), Y2(i,0))}


    val series3 = new XYSeries("prediction df=3")
    index foreach { i => series3.add(X(0,i), Y3(i,0))}


    series.addSeries(seriesA)

    series.addSeries(series1)
    series.addSeries(series2)
    series.addSeries(series3)

    val chart = XYLineChart(series)
    chart.title = "Example sin wave approximation"
    chart.show()

  }

}

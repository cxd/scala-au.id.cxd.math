import java.io.{File, PrintWriter}

import scala.collection.mutable
import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis._
import breeze.linalg.{DenseMatrix, DenseVector, eigSym, inv, sum, svd}
import au.id.cxd.math.function.distance.{Cor, Cov, MahalanobisDistance}
import au.id.cxd.math.probability.continuous.ChiSquare


val fileName:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/iris_virginica.csv"

new File(fileName).getAbsolutePath

val mat = MatrixReader.readFileAt(fileName)
// extract the columns of interest.
//val data = StandardisedNormalisation().transform( mat(::, 0 to 3) )

val data = mat(::, 0 to 3)

// get the distance measures for each example.
val distance = MahalanobisDistance(data)

// sort distance from lowest to highest.
val distSorted = distance.toArray.sorted
val n = distSorted.length

// chisquare quantiles
val x = for (i <- 1.0 to n.toDouble by 1.0) yield i/n
val chisq = ChiSquare(2.0)
val quantiles = x.map(chisq.invcdf(_))

import vegas._

val maxY = Math.round(quantiles.filter(q => q < Double.PositiveInfinity).max)

val abline = (for (i <- 0.0 to n by 1.0) yield (i/maxY)*0.5)



val plotData = distSorted
  .zip(quantiles)
    .zip(abline)
    .map { group => (group._1._1, group._1._2, group._2) }
  .map {
  pair => Map("distance" -> pair._1,
    "quantile" -> pair._2,
  "abline" -> pair._3)
}

val plot = Vegas.layered("Quantiles for chisq distribution",
  width=800.0,
  height=600.0).
  withData(
    plotData
  ).
  withLayers(
    Layer().
      mark(Point).
        encodeX("distance", Quantitative).
        encodeY("quantile", Quantitative),
    Layer().
      mark(Line).
      encodeX("abline", Quantitative).
      encodeY("abline", Quantitative).encodeColor(value="red")
  )

VegasHelper.showPlot(plot, fileName="docs/plots/mahalanobis.html")
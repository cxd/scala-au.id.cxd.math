import java.io.{File, PrintWriter}

import scala.collection.mutable
import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.data.MatrixReader
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

import vegas.render.HTMLRenderer._
import vegas.render._

val plotData = distSorted.zip(quantiles).map {
  pair => Map("distance" -> pair._1, "quantile" -> pair._2)
}.seq

val plot = Vegas("Quantiles for chisq distribution",
  width=800.0,
  height=600.0).
  withData(
    plotData
  ).
  mark(Point).
  encodeX("distance", Quantitative).
  encodeY("quantile", Quantitative)


val plotFrame = plot.pageHTML()
val fileout = new File("temp.html")
val writer = new PrintWriter(fileout)
writer.write(plotFrame)
writer.close()

val browser = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

//  Runtime.getRuntime.exec(
//  Array(browser, fileout.getAbsolutePath))


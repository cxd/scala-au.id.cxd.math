import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.model.sequence.SingularSpectrumAnalysis
import vegas.spec.Spec.MarkEnums.Bar
import vegas.spec.Spec.TypeEnums.Nominal
import vegas.{Layer, Line, Point, Quantitative, Vegas}

/*

This example illustrates the use of SSA on
precipitation data recorded by the station at Cape Moreton in Queensland
just off the coast of Brisbane.

The SSA procedure slides the signal over a window
of 365 days and the resulting eigenvectors provide
the oscillation of the signal over the period.

The primary eigenvectors are trignometric functions
which are out of phase with each other by pi/2

or npi/2L where L is the period.

 */

val fileName: String = "/Users/cd/Projects/scala/au.id.cxd.math/data/precipitation/example_cape_moreton.csv"

new File(fileName).getAbsolutePath

val mat = MatrixReader.readFileAt(fileName, cols = Seq(1))
val data = mat(::, 0).toArray
// estimating SSA with 365 days per stride
val (eVals, eVec, varExp, proj) = SingularSpectrumAnalysis(data, 365, scale = false)

varExp(0 to 10)

val numComponents = varExp.length
val plotData = varExp
  .toArray
  .zip((1 to numComponents))
  .map {
    pair =>
      Map("component" -> s"C${pair._2}",
        "variance" -> 100.0 * pair._1)
  }

val plot = Vegas.layered("Variance explained by component",
  width = 1024.0,
  height = 600.0).
  withData(
    plotData
  ).
  withLayers(
    Layer().
      mark(Bar).
      encodeX("component", Nominal).
      encodeY("variance", Quantitative)
  )
VegasHelper.showPlot(plot,
  fileName = "docs/plots/precip_varexplained.html")

val plotData2 = eVec(::, 0).toArray
  .zip(eVec(::, 1).toArray)
  .zip(1 to eVec.rows)
.map {
  tuple1 => {
    val e1 = tuple1._1._1
    val e2 = tuple1._1._2
    val day = tuple1._2
    Map(
      "eigenVec1" -> e1,
      "eigenVec2" -> e2,
      "combined" -> (e1 + e2),
      "day" -> day
    )
  }
}


val plot2 =
  Vegas.layered("Eigenvectors 1 and 2",
  width=1024.0,
  height=600.0).
  withData(
    plotData2
  ).
  withLayers(
    Layer().
      mark(Point).
      encodeX("day", Nominal).
      encodeY("eigenVec1", Quantitative)
    .encodeColor(value="blue"),
    Layer().
      mark(Line).
      encodeX("day", Nominal).
      encodeY("eigenVec2", Quantitative)
      .encodeColor(value="red"),
    Layer().
      mark(Line).
      encodeX("day", Nominal).
      encodeY("combined", Quantitative)
      .encodeColor(value="black")
  )

VegasHelper.showPlot(plot2,
  fileName = "docs/plots/precip_eigenvec1_2.html")

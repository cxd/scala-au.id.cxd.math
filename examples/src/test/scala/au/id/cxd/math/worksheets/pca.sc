import java.io.{File, PrintWriter}

import au.id.cxd.math.data.MatrixReader

import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.model.components.PrincipleComponentsAnalysis
import vegas.DSL.ExtendedUnitSpecBuilder
import vegas.spec.Spec.MarkEnums.Bar
import vegas.spec.Spec.TypeEnums.Nominal
import vegas.{Point, Quantitative, Vegas}
// experimenting with PCA on sparrows data


val file2:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/test_sparrows.csv"
val mat2 = MatrixReader.readFileAt(file2)

val data2 = mat2(::, 1 to 5).toDenseMatrix


val (eigenValues, eigenVectors, varExplained, projection) = PrincipleComponentsAnalysis(data2)

val v = eigenValues.toDenseMatrix
v.cols
v.rows

// plot the variance explained.
val lastComponent = eigenValues.length + 1
val components = (for (i <- 1 to lastComponent) yield i)
                    .toArray
                  .zip(eigenValues.toArray)
                  .zip(varExplained.toArray.map(v => v*100))
                  .map { group => (group._1._1, group._1._2, group._2)}


val componentData = components.map {
  comp => Map("component" -> comp._1,
    "eigenValue" -> comp._2,
    "percentVariance" -> comp._3)
}


import vegas.render._
val plot1 = Vegas("Percent Variance Explained for Component",
  width=800.0,
  height=600.0).
  withData(
    componentData
  ).
  mark(Bar).
  encodeX("component", Nominal).
  encodeY("percentVariance", Quantitative)

VegasHelper.showPlot(plot1, fileName="docs/plots/plotpca1.html")



// plot the first two principle components.
val c1 = projection(::,0).toArray
val c2 = projection(::,1).toArray
val survived = mat2(::, 6).toArray

// we are going to create a scatter plot of the two
// components coloured by survived or not
val dataset = c1.zip(c2).zip(survived)
.map { group => (group._1._1, group._1._2, group._2) }
.map { group => Map("c1" -> group._1,
  "c2" -> group._2,
  "survived" -> group._3)}


val plot2 = Vegas("Ordination of sparrows vs survival",
  width=800.0,
  height=600.0).
  withData(
    dataset
  ).
  mark(Point).
  encodeX("c1", Quantitative).
  encodeY("c2", Quantitative).
  encodeColor(field="survived", dataType=Nominal)

VegasHelper.showPlot(plot2, fileName="docs/plots/plotpca2.html")
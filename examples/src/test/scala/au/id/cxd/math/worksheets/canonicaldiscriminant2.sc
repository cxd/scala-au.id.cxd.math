import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import vegas.DSL.Layer
import vegas.spec.Spec.MarkEnums.{Square, Text}
import vegas.{Point, Quantitative, Vegas}
import vegas.spec.Spec.TypeEnums.Nominal

import scala.io.Source
// investigate the use of canonical discriminant analysis.



val file:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/test_mandible_data.csv"
val mat = MatrixReader.readFileAt(file)
val m2 = mat(::, 2 to 10).toDenseMatrix
val X = StandardisedNormalisation().transform(m2)
// we also know ahead of time that there are 5 groups in the data.
val groups = mat(::,1).toArray.map(_.toString).toList


val (components, coeffs, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, X)

percentVar

val attributes = List(
  "X1",
  "X2",
  "X3",
  "X4",
  "X5",
  "X6",
  "X7",
  "X8",
  "X9"
)
val componentNames = (for (i <- 1 to cor.cols) yield i.toString)

val corData = (for (i <- 0 until attributes.length) yield i)
  .foldLeft(List[Map[String, Any]]()) {
    (accum1, i) => {
      (for (j <- 0 until componentNames.length) yield j)
        .foldLeft(accum1) {
          (accum, j) => {
            val d = cor(i,j)
            val record = Map(
              "component" -> componentNames(j),
              "attribute" -> attributes(i),
              "correlation" -> d
            )
            accum :+ record
          }
        }
    }
  }


// looking at the correlation plot the first and fourth
// dimensions look quite interesting
val comp1 = zMat(::,0).toArray
val comp2 = zMat(::,4).toArray
// note however there is not alot of linear separation as one component
// seems to be described mystely by the absence of attributes, most strngly X2
// and the other seems to be defined by the presence of elements
// most likely X1


// note also inspecting the ordination there seems to be some separability
// between the groups. although group 5 is least separable from groups 1 and 3.
// there is good separation between 2, 1, 3 and 4.


val n = zMat.rows
val m = zMat.cols

// the length of the matrix corresponds to the number of distinct groups.
val dataset = comp1.zip(comp2).zip(groups).
  map { group => (group._1._1, group._1._2, group._2) }.
  map { group => Map("c1" -> group._1,
    "c2" -> group._2,
    "group" -> group._3)}


val plot1 = Vegas.layered("Ordination of Mandible Data",
  width=800.0,
  height=600.0).
  withData(
    dataset
  ).withLayers(
  /*Layer().
    mark(Point).
      encodeX("c1", Quantitative).
      encodeY("c2", Quantitative).
      encodeColor(field="country", dataType=Nominal).
      encodeText(field="country", dataType=Nominal),
  */
  Layer().
    mark(Text).
    encodeX("c1", Quantitative).
    encodeY("c2", Quantitative).
    encodeColor(field="group", dataType=Nominal).
    encodeText(field="group", dataType=Nominal)
)

VegasHelper.showPlot(plot1, fileName="plotcdfmandible.html")


// display a heatmap of attributes and correlation
// to the associated canonical component
val plot2 = Vegas("Heatmap of attribute and component correlation for mandible data",
  width = 600d,
  height = 600d).
  withData(
    corData
  ).
  mark(Square).
  encodeX("component", Nominal).
  encodeY("attribute", Nominal).
  encodeColor(field="correlation", dataType=Quantitative)
/*.
configScale(
  sequentialColorRange=SequentialColorRangeListString(
    List("inferno")))
*/


VegasHelper.transformAndShowPlot(plot2,
  VegasHelper.replaceMark("rect", _),
  fileName="plotcormandible.html")

val groupNames = List[String](
  "1.0", "2.0", "3.0", "4.0", "5.0"
)

// while we have not partitioned the data,
// we can still reclassify the original data
// to get some indication of the prediction that the
// model will make based on minimum distances from the group means
val (yProjection, groupProjection, distances, groupAssignments) = CanonicalDiscriminantAnalysis.classify(X, coeffs, groupMeans, groupNames)

// and we can plot the predicted groups using the coordinates of the resulting projection.
val p1 = yProjection(::,0).toArray
val p2 = yProjection(::,4).toArray

groupAssignments

val dataset2 = p1.zip(p2).zip(groupAssignments).
  map { group => (group._1._1, group._1._2, group._2._1, group._2._2) }.
  map { group => Map("p1" -> group._1,
    "p2" -> group._2,
    "group" -> group._4)}


val plot3 = Vegas.layered("Predicted Ordination of Mandible Data",
  width=800.0,
  height=600.0).
  withData(
    dataset2
  ).withLayers(
  /*Layer().
    mark(Point).
      encodeX("c1", Quantitative).
      encodeY("c2", Quantitative).
      encodeColor(field="country", dataType=Nominal).
      encodeText(field="country", dataType=Nominal),
  */
  Layer().
    mark(Text).
    encodeX("p1", Quantitative).
    encodeY("p2", Quantitative).
    encodeColor(field="group", dataType=Nominal).
    encodeText(field="group", dataType=Nominal)
)

VegasHelper.showPlot(plot3, fileName="plotpredictmandible.html")

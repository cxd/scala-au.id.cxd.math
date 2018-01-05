import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import vegas.{Point, Quantitative, Vegas}
import vegas.spec.Spec.TypeEnums.Nominal

import scala.io.Source
// investigate the use of canonical discriminant analysis.



val file:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/employ_pop.csv"
val mat = MatrixReader.readFileAt(file)

val file2:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/employ_countries.csv"
val countries = Source.fromFile(file2).getLines().toArray.tail



val m2 = mat(::, 1 to 10).toDenseMatrix

//println(m2)
// the data set is standardised prior to the procedure
val X = StandardisedNormalisation().transform(m2)
// we also know ahead of time that there are 5 groups in the data.
val groups = mat(::,0).toArray.map(_.toString).toList

// perform the analysis.

val (components, coeffs, percentVar, zMat, cor) = CanonicalDiscriminantAnalysis(groups, X)

// plot the ordination.
val comp1 = zMat(::,0).toArray
val comp2 = zMat(::,1).toArray

val n = zMat.rows
val m = zMat.cols

// the length of the matrix corresponds to the number of distinct groups.
val dataset = comp1.zip(comp2).zip(countries).
  map { group => (group._1._1, group._1._2, group._2) }.
  map { group => Map("c1" -> group._1,
    "c2" -> group._2,
  "country" -> group._3)}


val plot2 = Vegas("Ordination of CDF",
  width=800.0,
  height=600.0).
  withData(
    dataset
  ).
  mark(Point).
  encodeX("c1", Quantitative).
  encodeY("c2", Quantitative).
  encodeColor(field="country", dataType=Nominal)

VegasHelper.showPlot(plot2, name="plotcdf.html")
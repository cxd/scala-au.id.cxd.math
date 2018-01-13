import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import vegas.DSL.Layer
import vegas.spec.Spec.MarkEnums.{Square, Text}
import vegas.{Point, Quantitative, Vegas}
import vegas.spec.Spec.TypeEnums.Nominal


val file:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/test_mandible_train.csv"
val file2:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/test_mandible_test.csv"
val mat = MatrixReader.readFileAt(file)
val m2 = mat(::, 2 to 10).toDenseMatrix
val X = StandardisedNormalisation().transform(m2)
// we also know ahead of time that there are 5 groups in the data.
val groups = mat(::,1).toArray.map(_.toString).toList
val uniqueGroups = groups.sorted.distinct

val mat2 = MatrixReader.readFileAt(file2)
val m3 = mat(::,2 to 10).toDenseMatrix
val Y = StandardisedNormalisation().transform(m3)
val groupsY = mat(::,1).toArray.map(_.toString).toList



val (components, coeffs, intercept, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, X)

val result1 = CanonicalDiscriminantAnalysis.classify(Y, coeffs, intercept, groupMeans, uniqueGroups)

val result2 = CanonicalDiscriminantAnalysis.classifyDiscriminant(Y, coeffs, intercept, groupMeans, uniqueGroups)

val test1 = result1._4.map(_._2)
val crosstab1 = CrossTabulate(groupsY, test1)
val r1 = CrossTabulate.metrics(crosstab1)

val test2 = result2._4.map(_._2)
val crosstab2 = CrossTabulate(groupsY, test2)
val r2 = CrossTabulate.metrics(crosstab2)
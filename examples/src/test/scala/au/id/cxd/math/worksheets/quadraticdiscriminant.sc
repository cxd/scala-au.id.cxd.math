import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.example.charting.VegasHelper
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.{CanonicalDiscriminantAnalysis, QuadraticDiscriminant}
import vegas.DSL.Layer
import vegas.spec.Spec.MarkEnums.Text
import vegas.spec.Spec.TypeEnums.Nominal
import vegas.{Quantitative, Vegas}

val file1:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/wine/wine_data_train.csv"

val file2:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/wine/wine_data_test.csv"

val mat1 = MatrixReader.readFileAt(file1)
val mat2 = MatrixReader.readFileAt(file2)

// 1st column is group
// 14 columns
val trainGroups = mat1(::,0).toArray.map(_.toString).toList
val testGroups = mat2(::,0).toArray.map(_.toString).toList
val temp1 = mat1(::,1 to 13)
val temp2 = mat2(::,1 to 13)
val trainData = StandardisedNormalisation().transform(temp1)
val testData = StandardisedNormalisation().transform(temp2)


// build the canonical discriminant model.
val quadParams = QuadraticDiscriminant(trainData, trainGroups)

// perform the test classification.
val predictedClasses = QuadraticDiscriminant.classifyDiscriminant(testData, quadParams)

val predictGroups = predictedClasses.map(_._1)

val crosstab = CrossTabulate(testGroups, predictGroups)

val metrics = CrossTabulate.metrics(crosstab)

// we will need to use a different type of ordination
// in order to display the data in a reduced dimensionality
// using the LDA for ordination
val ordination = CanonicalDiscriminantAnalysis(testGroups, testData)
val projection = ordination._5

val ordX = projection(::,0).toArray
val ordY = projection(::,1).toArray

// label the ordination with the corresponding group assignment
val dataset = ordX.zip(ordY).zip(testGroups).
  map { group => (group._1._1, group._1._2, group._2) }.
  map { group => Map("x" -> group._1,
    "y" -> group._2,
    "origin" -> group._3)}



val plot1 = Vegas.layered("Ordination of Wine Test Data",
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
    encodeX("x", Quantitative).
    encodeY("y", Quantitative).
    encodeColor(field="origin", dataType=Nominal).
    encodeText(field="origin", dataType=Nominal)
)

VegasHelper.showPlot(plot1, fileName="docs/plots/qdawine_example.html")

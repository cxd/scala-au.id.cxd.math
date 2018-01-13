package model.components

import java.io.File

import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import breeze.linalg.DenseMatrix
import inference.TestManovaData
import org.scalatest.{FlatSpec, Matchers}

class TestCDF extends FlatSpec with Matchers {


  "CDF" should "extract canonical functions" in new TestEmployPopData {
    val (groups, data) = read()
    val (components, coeffs, intercept, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, data)

    println(s"$percentVar")
    println("Correlations")
    println(s"$cor")
  }

  "CDF" should "perform classification task" in new TestManovaData {
    val (groups, data) = read()
    val (components, coeffs, intercept, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, data)
    val result = CanonicalDiscriminantAnalysis.classify(data, coeffs, intercept, groupMeans, groups)
    println(result._4)
  }

  "CDF" should "perform 1 classification discriminant task" in new TestManovaData {
    val (groups, data) = read()
    val uniqueGroups = groups.sorted.distinct
    val (components, coeffs, intercept, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, data)
    val result = CanonicalDiscriminantAnalysis.classifyDiscriminant(data(0,::).inner.toDenseMatrix, coeffs, intercept, groupMeans, uniqueGroups)
    val test = result._4.map(_._2)
    val crosstab = CrossTabulate[String](groups.toSeq, test.toSeq)
    println(result)
    println(crosstab)
  }

  "CDF" should "perform all classification discriminant task" in new TestManovaData {
    val (groups, data) = read()
    val uniqueGroups = groups.sorted.distinct
    val (components, coeffs, intercept, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, data)
    val result = CanonicalDiscriminantAnalysis.classifyDiscriminant(data, coeffs, intercept, groupMeans, uniqueGroups)
    val test = result._4.map(_._2)
    val crosstab = CrossTabulate(groups.toSeq, test.toSeq)
    println(result)
    println(crosstab)
    val m = CrossTabulate.metrics(crosstab)
    println(m)
  }


}



trait TestEmployPopData extends MatrixReader {

  val fileName="employ_pop.csv"

  override val skipHeader = true

  def read():(List[String], DenseMatrix[Double]) = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)

    val m2 = mat(::, 1 to 9).toDenseMatrix

    //println(m2)
    // the data set is standardised prior to the procedure
    val X = StandardisedNormalisation().transform(m2)
    // we also know ahead of time that there are 5 groups in the data.
    val groups = mat(::,0).toArray.map(_.toString).toList

    //println(groups)
    (groups, X)
  }
}

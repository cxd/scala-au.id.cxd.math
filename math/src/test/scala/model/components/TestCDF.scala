package model.components

import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import breeze.linalg.DenseMatrix
import org.scalatest.{FlatSpec, Matchers}

class TestCDF extends FlatSpec with Matchers {


  "CDF" should "extract canonical functions" in new TestEmployPopData {
    val (groups, data) = read()
    val (components, coeffs, percentVar, zMat, cor, groupMeans) = CanonicalDiscriminantAnalysis(groups, data)

    println(s"$percentVar")
    println("Correlations")
    println(s"$cor")
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

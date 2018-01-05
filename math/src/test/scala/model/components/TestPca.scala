package model.components

import au.id.cxd.math.model.components.PrincipleComponentsAnalysis
import inference.TestManovaDataSparrows
import org.scalatest.{FlatSpec, Matchers}

class TestPca extends FlatSpec with Matchers {

  "PCA" should "decompose data" in new TestManovaDataSparrows {
    val (group,data) = read()
    val (eigenValues, eigenVectors, varianceExplained, projection) = PrincipleComponentsAnalysis(data)
  }

}

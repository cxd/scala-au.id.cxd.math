package inference

import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis._
import breeze.linalg.DenseMatrix
import org.scalatest.{FlatSpec, Matchers}


class TestManovaSparrows extends FlatSpec
with Matchers {

  "Manova" should "partition groups" in new TestManovaDataSparrows {
    val (groups, data) = read()

    val m = Manova.build(groups, data)

    val idx = m.groupIndexes(groups)

    val p = m.partitions

    // there are 5 groups in the test data.
    println(s"Group Length: ${idx.size}")
    println(s"Partitions: ${p.length}")

    p.length should be(2)

  }

  "Test Wilks" should "accept H_0" in new TestManovaDataSparrows {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=WilksLambda())

    println(result)

    result.reject should be (false)
  }

  "Test Pillais" should "accept H_0" in new TestManovaDataSparrows {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=PillaisTrace())

    println(result)

    result.reject should be (false)
  }

  "Test Roys" should "accept H_0" in new TestManovaDataSparrows {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=RoysLargestRoot())

    println(result)

    result.reject should be (false)
  }

  "Test HotellingLawley" should "accept H_0" in new TestManovaDataSparrows {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=LawesHotellingTrace())

    println(result)

    result.reject should be (false)
  }
}


trait TestManovaDataSparrows extends MatrixReader {

  val fileName="test_sparrows.csv"

  val expectedGroups :Int = 2

  override val skipHeader = true

  def read():(List[String], DenseMatrix[Double]) = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)
    // we know the headers on the first line
    // they are:
    // "Case","Group","X1","X2","X3","X4","X5","X6","X7","X8","X9","Sex"
    // we want to keep columns 2 .. 10 and discard the other three columns including the last.
    val m2 = mat(::, 1 to 5).toDenseMatrix

    //println(m2)
    // the data set is standardised prior to the procedure
    val X = StandardisedNormalisation().transform(m2)
    // we also know ahead of time that there are 5 groups in the data.
    val groups = mat(::,6).toArray.map(_.toString).toList

    //println(groups)
    (groups, X)
  }
}
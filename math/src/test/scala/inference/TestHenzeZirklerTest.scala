package inference

import au.id.cxd.math.probability.analysis.HenzeZirklerTest
import org.scalatest.{FlatSpec, Matchers}

class TestHenzeZirklerTest extends FlatSpec with Matchers {

  "HZ test" should "not reject null hypothesis" in new MardiaIrisSubset {
    val data = read()
    val result = HenzeZirklerTest(0.05, data)

    println(result.toString)

    result.rejectTest should be(false)
  }


  "HZ test" should "reject null hypothesis" in new TestManovaData {
    val (group, data) = readNonStandardised()
    val result = HenzeZirklerTest(0.05, data)

    println(result.toString)

    result.rejectTest should be(true)
  }

  "HZ test" should "reject all iris H0" in new MardiaIrisAll {
    val data = read()
    val result = HenzeZirklerTest(0.05, data)

    println(result.toString)

    result.rejectTest should be(true)

  }
  //TestManovaDataSparrows

  "HZ test" should "not reject null hypothesis with sparrow data" in new TestManovaDataSparrows {
    val (group,data) = read()
    val result = HenzeZirklerTest(0.05, data)

    println(result.toString)

    result.rejectTest should be(false)
  }

}

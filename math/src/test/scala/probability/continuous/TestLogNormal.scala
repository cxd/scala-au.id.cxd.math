package probability.continuous

import au.id.cxd.math.probability.continuous.{Gamma, LogNormal}
import function.TestEvaluation
import org.scalatest.{FlatSpec, Matchers}

class TestLogNormal extends FlatSpec with Matchers with TestEvaluation {
  val quantiles = List[Double](
    0.1,
    0.2,
    0.3,
    0.4,
    0.5,
    0.6,
    0.7,
    0.8,
    0.9,
    1.0
  )

  val pdftest = List[Double](
    0.2815902,
    0.5462679,
    0.6442033,
    0.6554442,
    0.6274961,
    0.5835738,
    0.5347948,
    0.4864158,
    0.4408157,
    0.3989423
  )


  val cdftest = List[Double](
    0.01065110,
    0.05376031,
    0.11430005,
    0.17975721,
    0.24410860,
    0.30473658,
    0.36066758,
    0.41171189,
    0.45804487,
    0.50000000
  )

  "PDF" should "agree with R" in {
    val fn = LogNormal(0,1).pdf(_)
    println("LogNormal(0,1) PDF")
    evaluate1(fn, quantiles, pdftest, 0.01) should be (true)
    println()
  }

  "CDF" should "agree with R " in {
    val fn = (y:Double) => LogNormal(0,1).cdf(y)
    println("LogNormal(0,1) CDF")
    evaluate1(fn, quantiles, cdftest, 0.1) should be(true)
    println()
  }

  "INVCDF" should "agree with R" in {
    val fn = LogNormal(0,1).invcdf(_)
    println("LogNormal(0,1) INVCDF")
    evaluate1(fn, cdftest, quantiles, 0.1) should be(true)
    println()
  }
}

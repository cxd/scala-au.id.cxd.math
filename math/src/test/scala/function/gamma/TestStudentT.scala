package function.gamma

import au.id.cxd.math.probability.continuous.{Gamma, StudentT}
import function.TestEvaluation
import org.scalatest.{FlatSpec, Matchers}

class TestStudentT extends FlatSpec with Matchers with TestEvaluation {
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
    0.3151583,
    0.3060672,
    0.2920274,
    0.2744051,
    0.2546479,
    0.2340514,
    0.2136308,
    0.1940914,
    0.1758618,
    0.1591549
  )


  val cdftest = List[Double](
    0.5317255,
    0.5628330,
    0.5927736,
    0.6211189,
    0.6475836,
    0.6720209,
    0.6944001,
    0.7147767,
    0.7332623,
    0.7500000
  )

  "PDF" should "agree with R" in {
    val fn = StudentT(1.0).pdf(_)
    println("StudentT PDF")
    evaluate1(fn, quantiles, pdftest, 0.01) should be (true)
    println()
  }

  "CDF" should "agree with R " in {
    val fn = (y:Double) => StudentT(1.0).cdf(y)
    println("StudentT CDF")
    evaluate1(fn, quantiles, cdftest, 0.01) should be(true)
    println()
  }

  "INVCDF" should "agree with R" in {
    val fn = StudentT(1.0).invcdf(_)
    println("StudentT INVCDF")
    evaluate1(fn, cdftest, quantiles, 0.01) should be(true)
    println()
  }
}

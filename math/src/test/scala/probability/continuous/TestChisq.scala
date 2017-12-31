package probability.continuous

import au.id.cxd.math.probability.continuous.{ChiSquare, StudentT}
import function.TestEvaluation
import org.scalatest.{FlatSpec, Matchers}

class TestChisq extends FlatSpec with Matchers with TestEvaluation {
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
    1.2000389,
    0.8071711,
    0.6269101,
    0.5164415,
    0.4393913,
    0.3815453,
    0.3360145,
    0.2989835,
    0.2681367,
    0.2419707
  )


  val cdftest = List[Double](
    0.2481704,
    0.3452792,
    0.4161176,
    0.4729107,
    0.5204999,
    0.5614220,
    0.5972163,
    0.6289066,
    0.6572183,
    0.6826895
  )

  "PDF" should "agree with R" in {
    val fn = ChiSquare(1.0).pdf(_)
    println("ChiSquare PDF")
    evaluate1(fn, quantiles, pdftest, 0.01) should be (true)
    println()
  }

  "CDF" should "agree with R " in {
    val fn = (y:Double) => ChiSquare(1.0).cdf(y)
    println("ChiSquare CDF")
    evaluate1(fn, quantiles, cdftest, 0.01) should be(true)
    println()
  }

  "INVCDF" should "agree with R" in {
    val fn = ChiSquare(1.0).invcdf(_)
    println("ChiSquare INVCDF")
    evaluate1(fn, cdftest, quantiles, 0.01) should be(true)
    println()
  }
}

package probability.continuous

import au.id.cxd.math.probability.continuous.Gamma
import function.TestEvaluation
import org.scalatest.{FlatSpec, Matchers}

class TestGamma extends FlatSpec with Matchers with TestEvaluation {

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
    0.75549201,
    0.36633080,
    0.23012397,
    0.16072652,
    0.11897044,
    0.09135798,
    0.07195566,
    0.05773548,
    0.04698682,
    0.03866917
  )


  val cdftest = List[Double](
    0.8275518,
    0.8794196,
    0.9083580,
    0.9275738,
    0.9414024,
    0.9518321,
    0.9599448,
    0.9663947,
    0.9716069,
    0.9758727
  )

  "PDF" should "agree with R" in {
    val fn = Gamma(0.1)(1.0).pdf(_)
    println("Gamma PDF")
    evaluate1(fn, quantiles, pdftest, 0.01) should be (true)
    println()
  }

  "CDF" should "agree with R " in {
    val fn = (y:Double) => Gamma(0.1)(1.0).cdf(y)
    println("Gamma CDF")
    evaluate1(fn, quantiles, cdftest, 0.1) should be(true)
    println()
  }

  "INVCDF" should "agree with R" in {
    val fn = Gamma(0.1)(1.0).invcdf(_)
    println("Gamma INVCDF")
    evaluate1(fn, cdftest, quantiles, 0.1) should be(true)
    println()
  }
}

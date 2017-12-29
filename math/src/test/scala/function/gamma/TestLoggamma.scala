package function.gamma

import au.id.cxd.math.function.gamma.LogGammaFn
import org.scalatest.{FlatSpec, ShouldMatchers}

class TestLoggamma extends FlatSpec with ShouldMatchers {

  "Log gamma" should "return result" in {
    val (y1, err1) = LogGammaFn(0.1)
    println(s"y1: $y1")
    (y1 >= 2.252712651734204 && y1 <= 2.252712651734206) should be(true)
  }
  "Log gamma" should "agree with mathematica" in {
    val results = List[(Double,Double)](
      (0.01, 4.59948),
      (0.11, 2.15324),
      (0.21, 1.47245),
      (0.31, 1.06137),
      (0.41, 0.771422),
      (0.51, 0.552974),
      (0.61, 0.383008),
      (0.71, 0.248808),
      (0.81, 0.142524),
      (0.91, 0.0589226),
      (0.7, 0.26086724653166651439),
      (0.1,2.2527126517342059599)
    )
    val lngamma = results.map { pair =>
      val test = LogGammaFn(pair._1)
      println(s"LogGamma[${pair._1}] = ${pair._2} ~ ${test._1}")
      Math.abs(test._1 - pair._2) < 0.001 should be (true)
    }

  }

}

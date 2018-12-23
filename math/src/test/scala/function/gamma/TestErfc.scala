package function.gamma

import au.id.cxd.math.function.gamma.Erfc
import org.scalatest.{FlatSpec, Matchers}

class TestErfc extends FlatSpec with Matchers {

  "Erfc" should "agree with GSL" in {
    val data = List[(Double,Double)](
      (-10.0, 2.0),
      (-5.0000002, 1.9999999999984625433),
      (-1.0, 1.8427007929497148693),
      (-0.5, 1.5204998778130465377),
      (1.0, 0.15729920705028513066),
      (3.0, 0.000022090496998585441373),
      (7.0, 4.183825607779414399e-23),
      (10.0, 2.0884875837625447570e-45)
    )
    val y = data.map(_._1).map(Erfc(_))
    val pairs = data.map(_._2).zip(y)
    pairs.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
  }

}

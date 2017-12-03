package function.hypergeometric

import org.scalatest._
import au.id.cxd.math.function.hypergeometric._


class TestGaussHypergeometric extends FlatSpec with ShouldMatchers {

  "Hypergeometric Function" should "Agree with GSL" in {
    val results = List(2.0,
      12451584.0,
      4205.714285714285714,
      0.13671875)

    val test = List(GaussHypergeometric(1.0, 1.0, 1.0, 0.5),
      GaussHypergeometric(8, 8, 1, 0.5),
      GaussHypergeometric(8, 8, 5, 0.5),
      GaussHypergeometric(8, -8, 1, 0.5))

    val pairs = results.zip(test)

    pairs.forall(p => {
      println(s"result: ${p._2._1} approx ${p._1}")
      Math.abs(p._2._1 - p._1) < 0.1
    }) should be(true)
  }
}

package probability.continuous

import au.id.cxd.math.probability.continuous.Beta
import org.scalatest.{FlatSpec, ShouldMatchers}

class TestBeta extends FlatSpec with ShouldMatchers {

  // TODO: further debugging required on Inverse Beta function.
  "Quartiles of Beta distribution" should "agree with R" in {
    val betaDist = Beta(1.0,1.0)
    val results = List(0.5,
      0.1,
      0.9)
    val tests = List(
      betaDist.invcdf(0.5),
      betaDist.invcdf(0.1),
      betaDist.invcdf(0.9)
    )

    val pairs = results.zip(tests)

    pairs.foreach(p => println(s"${p._1} ~ ${p._2}"))

    pairs.forall {
      p => Math.abs(p._1 - p._2) < 0.01
    } should be (true)
  }
}

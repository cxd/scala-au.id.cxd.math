package function.beta

import au.id.cxd.math.function.beta.IncompleteBetaFn
import org.scalatest.{FlatSpec, ShouldMatchers}

class TestIncompleteBetaFn extends FlatSpec with ShouldMatchers {

  "Inverse beta" should "match with GSL tests" in {

    // x, alpha, beta note the gsl method order is a, b, x instead
    val fn = IncompleteBetaFn(_,_,_)

    val results = List(0.0,
      1.0,
      1.0,
      0.5)

    val test = List(fn(0.0,1.0,1.0),
      fn(1.0,1.0,1.0),
      fn(1.0, 0.1, 0.1),
      fn(0.5, 1.0, 1.0))

    val pairs = results.zip(test)

    pairs.forall(p => {
      println(s"result: ${p._2} approx ${p._1}")
      Math.abs(p._2 - p._1) < 0.1
    }) should be(true)

  }

}

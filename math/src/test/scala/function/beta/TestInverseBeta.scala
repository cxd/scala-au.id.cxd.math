package function.beta

import au.id.cxd.math.function.beta.InverseBeta
import org.scalatest.{FlatSpec, Matchers}

class TestInverseBeta extends FlatSpec with Matchers {

  "Inverse Beta" should "Agree with GSL" in {
    def fn(P:Double,a:Double,b:Double) = InverseBeta(P,a,b)
    val results = List(0.0, 1e-100, 0.001,0.01, 0.1, 0.325, 0.5, 0.9, 0.99, 1.0)
    val tests = List(fn(0.0,1.2,1.3),
      fn( 1.34434944656489596e-120, 1.2, 1.3),
      fn( 3.37630042504535813e-4, 1.2, 1.3),
      fn( 5.34317264038929473e-3, 1.2, 1.3),
      fn( 8.33997828306748346e-2, 1.2, 1.3),
      fn( 3.28698654180583916e-1, 1.2, 1.3),
      fn( 5.29781429451299081e-1, 1.2, 1.3),
      fn( 9.38529397224430659e-1, 1.2, 1.3),
      fn( 9.96886438341254380e-1, 1.2, 1.3),
      fn(1.0, 1.2, 1.3)
    )
    val pairs = results.zip(tests)
    pairs.foreach {
      pair => println(s"${pair._1} approx ${pair._2}")
    }
    val flag = pairs.forall {
      pair => Math.abs(pair._1 - pair._2) < 0.1
    }
    flag should be(true)
  }

}

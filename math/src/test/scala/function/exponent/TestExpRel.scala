package function.exponent

import au.id.cxd.math.function.exponential.ExpRel
import org.scalatest.{FlatSpec, Matchers}

class TestExpRel extends FlatSpec with Matchers {

  "ExpRel" should "agree with GSL" in {
    val result = List[Double](
      385.425369029433473098652465720
    )
    val fn = ExpRel(_,_)
    val tests = List[Double](
      fn(6.315655e+05, 6.302583168053568806e+05)
    )

    val pairs = result.zip(tests)
    pairs.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
    pairs.forall{
      pair => Math.abs(pair._1 - pair._2) < 0.01
    } should be (true)
  }

}

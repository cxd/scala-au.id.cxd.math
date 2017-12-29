package function.gamma

import au.id.cxd.math.function.gamma.IncompleteGamma
import org.scalatest.{FlatSpec, Matchers}

class TestIncompleteGamma extends FlatSpec with Matchers {

  def evaluate(fn:(Double, Double) => Double, params:List[(Double,Double)], results:List[Double], threshold:Double) = {
    val output = params.map { pair => fn(pair._1, pair._2)}
    val tests = results.zip(output)
    tests.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
    tests.forall {
      pair => Math.abs(pair._1 - pair._2) < threshold
    }
  }


  "Incomplete Gamma P" should "agree with GSL gamma_inc_P" in {
    val fn = IncompleteGamma.P(_,_)
    val params = List[(Double,Double)](
      (1e-100, 0.001),
      (0.001, 0.001),
      (0.001, 1.0),
      (1.0, 0.001),
      (1.0, 1.01),
      (1.0, 10.0),
      (10.0, 10.01),
      (10.0, 20.0),
      (1000.0, 1000.1),
      (1000.0, 2000.0),
      (34.0, 32.0),
      (37.0, 3.499999999999999289e+01),
      (10, 1e-16),
      (1263131.0, 1261282.3637),
      (1263131.0, 1263131.0)
    )
    val results = List[Double](
      1.0,
      0.9936876467088602902,
      0.9997803916424144436,
      0.0009995001666250083319,
      0.6357810204284766802,
      0.9999546000702375151,
      0.5433207586693410570,
      0.9950045876916924128,
      0.5054666401440661753,
      1.0,
      0.3849626436463866776322932129,
      0.3898035054195570860969333039,
      2.755731922398588814734648067e-167,
      0.04994777516935182963821362168,
      0.500118321758657770672882362502514254
    )
    println("Incomplete Gamma P")
    evaluate(fn, params, results, 0.1) should be (true)

  }

  "Incomplete Gamma Q" should "agree with GSL gamma_inc_P" in {
    val fn = IncompleteGamma.Q(_,_)
    val params = List[(Double,Double)](
      (0.0, 0.001),
      (0.001, 0.001),
      (0.001, 1.0),
      (0.001, 2.0),
      (0.001, 5.0),
      (1.0, 0.001),
      (1.0, 1.01),
      (1.0, 10.0),
      (10.0, 10.01),
      (10.0, 100.0),
      (1000.0, 1000.1),
      (1000.0, 2000.0),
      (100,  99.0),
      (200, 199.0),
      (100,  99.0),
      (200, 199.0)
    )
    val results = List[Double](
      0.0,
      0.006312353291139709793,
      0.00021960835758555639171,
      0.00004897691783098147880,
      1.1509813397308608541e-06,
      0.9990004998333749917,
      0.3642189795715233198,
      0.00004539992976248485154,
      0.4566792413306589430,
      1.1253473960842733885e-31,
      0.4945333598559338247,
      6.847349459614753180e-136,
      0.5266956696005394,
      0.5188414119121281,
      0.4733043303994607,
      0.4811585880878718
    )
    println("Incomplete Gamma Q")

    evaluate(fn, params, results, 0.1) should be (true)

  }
}

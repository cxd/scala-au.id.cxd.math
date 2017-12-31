package function.gamma

import au.id.cxd.math.function.gamma.InverseGamma
import function.TestEvaluation
import org.scalatest.{FlatSpec, Matchers}

class TestInverseGamma extends FlatSpec with Matchers with TestEvaluation {

  "inverse gamma CDF Q" should "agree with GSL" in {
    val fn = (y:Double) => InverseGamma.Q(y, 1.0, 1.0)

    val parms = List[Double](
      2.06115362243855783e-9,
      4.53999297624848515e-5,
      1.35335283236612692e-1,
      2.23130160148429829e-1,
      3.67879441171442322e-1,
      7.22527353642072189e-1,
      9.04837418035959573e-1,
      9.90049833749168054e-1,
      9.99000499833374992e-1,
      1.0
    )

    val test = List[Double](
      20.0,
      10.0,
      2.0,
      1.5,
      1.0,
      0.325,
      0.1,
      0.01,
      0.001,
      0.0
    )
    println("Inverse Gamma Q")
    evaluate1(fn, parms, test, 0.1) should be(true)
    println()
  }


  "Inverse gamma P" should "agree with GSL" in {
    val fn = (y:Double) => InverseGamma.P(y, 1.0, 1.0)
    val parms = List[Double](
      9.99500166625008332e-4,
      9.95016625083194643e-3,
      9.51625819640404268e-2,
      2.77472646357927811e-1,
      6.32120558828557678e-1,
      7.76869839851570171e-1,
      8.64664716763387308e-1,
      9.99954600070237515e-1,
      9.99999997938846378e-1

    )
    val test = List[Double](
      0.001,
      0.01,
      0.1,
      0.325,
      1.0,
      1.5,
      2.0,
      10.0,
      20.0
    )
    println("Inverse Gamma P")
    evaluate1(fn, parms, test, 0.1) should be(true)
    println()
  }
}

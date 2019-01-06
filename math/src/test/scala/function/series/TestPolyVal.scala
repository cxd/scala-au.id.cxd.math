package function.series

import au.id.cxd.math.function.series.{ContinuedSeries, PolyVal}
import au.id.cxd.math.probability.continuous.Normal
import breeze.linalg.DenseVector
import org.scalatest.{FlatSpec, Matchers}

class TestPolyVal extends FlatSpec with Matchers {

  "polyval" should "evaluate polynomial" in {
    val n = 100

    val norm = Normal(0.0)(1.0)

    val i :Seq[Double] = for(i <- 0 until n) yield (i+1 - 3.0/8.0)/(n + 1.0/4.0)
    println(i)
    val mtilde1:DenseVector[Double] = DenseVector.tabulate[Double](n) {
      case i => {
        norm.invcdf((i+1 - 3.0/8.0)/(n + 1.0/4.0))
      }
    }
    val mtilde:Seq[Double] = mtilde1.data
    println(mtilde)

    val mdot = mtilde1.dot(mtilde1)
    println(mdot)
    val weights1 = 1.0 / Math.sqrt(mdot)
    val weights = mtilde.map { m => m * weights1 }
    val m = weights.last

    val c1:Seq[Double] = Array(m,.221157,-.147981,-2.07119, 4.434685, -2.706056)

    println(c1)

    val u = 1.0 / Math.sqrt(n)

    println(u)

    val test = PolyVal(c1.toList, u)

    println(test)
  }

}

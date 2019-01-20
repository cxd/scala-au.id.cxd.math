import au.id.cxd.math.function.series.ContinuedSeries
import au.id.cxd.math.probability.continuous.Normal

val n = 100

val c1:Seq[Double] = Array(0.0,.221157,-.147981,-2.07119, 4.434685, -2.706056)
val norm = Normal(0.0)(1.0)

val mtilde:Seq[Double] = for (i <- 1 to n) yield norm.invcdf((i - 3.0/8.0)/(i + 1.0/4.0))
val u = 1.0 / Math.sqrt(n)

val polyval = new ContinuedSeries {}

val test = polyval.poly(c1.toList, u)


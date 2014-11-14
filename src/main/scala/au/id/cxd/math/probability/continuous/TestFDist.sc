import breeze.linalg._
import breeze.numerics.pow

import au.id.cxd.math.probability.regression._
import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function._

val numeratorDf = 1.0
val denominatorDf = 2.0
val y = 1.2

val beta = BetaFn(numeratorDf/2.0)(denominatorDf/2.0)


val a = Math.pow(numeratorDf*y, numeratorDf)*Math.pow(denominatorDf, denominatorDf)
val b = Math.pow(numeratorDf*y + denominatorDf, numeratorDf+denominatorDf)
val c = Math.sqrt(a / b)
val d = y * beta
(c / d)

val a2 = Math.sqrt(Math.pow(denominatorDf, denominatorDf)) * Math.sqrt(Math.pow(numeratorDf, numeratorDf)) * Math.sqrt(Math.pow(y, numeratorDf-2.0))
val b2 = Math.sqrt(Math.pow(denominatorDf + numeratorDf*y, numeratorDf + denominatorDf)) * beta
a2 / b2

/**
> df(c(1, 1.1, 1.2), 2, 2)
[1] 0.2500000 0.2267574 0.2066116
> df(c(1, 1.1, 1.2), 2, 1)
[1] 0.1924501 0.1746928 0.1595077
  **/

val a3 = GammaFn((numeratorDf + denominatorDf)/2.0)

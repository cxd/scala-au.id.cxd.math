import breeze.linalg._
import breeze.numerics.pow

import au.id.cxd.math.probability.regression._
import au.id.cxd.math.count.Factorial
import au.id.cxd.math.function._

val numeratorDf = 1.0
val denominatorDf = 2.0
val y = 0.1

val beta = BetaFn(numeratorDf)(denominatorDf)

val a = Math.pow(numeratorDf * y, numeratorDf) * Math.pow(denominatorDf, denominatorDf)
val b = Math.pow(numeratorDf * y + denominatorDf, numeratorDf + denominatorDf)

val test = a / b

val c = Math.sqrt(a / b)
val d = y * beta
(c / d)

import Math._

import au.id.cxd.math.function.NumericIntegral

val limit = (0.0, 1.0)

def exampleNorm(mu:Double, sigma:Double)(y:Double):Double =
  1/(sqrt(2*Math.PI)*sigma) * exp(-(1/(2*pow(sigma,2.0)))*pow(y - mu, 2.0))

val fn : Double => Double =  exampleNorm(0.0, 1.0)(_)

val integral = NumericIntegral(-1.0, 1.0, fn)
val result = integral.integrate ()
val result2 = integral.integrateS ()
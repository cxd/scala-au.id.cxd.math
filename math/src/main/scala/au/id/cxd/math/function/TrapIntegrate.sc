import Math._

import au.id.cxd.math.function.approximate.NumericIntegral
import au.id.cxd.math.probability.continuous.Normal
val limit = (0.0, 1.0)
def exampleNorm(mu:Double, sigma:Double)(y:Double):Double =
  1/(sqrt(2*PI)*sigma) * exp(-(1/(2*pow(sigma,2.0)))*pow(y - mu, 2.0))
val fn : Double => Double =  exampleNorm(0.0, 1.0)(_)
// standard normal distribution p-value between -1.0 and 1.0
// P(x < 1.0) - P(x < -1.0) = pnorm(1.0) - pnorm(-1.0)
/*
> pnorm(1.0, 0.0, 1) - pnorm(-1.0, 0, 1)
[1] 0.6826895
 */
val integral = NumericIntegral(-1.0, 1.0, fn)
val result = integral.integrate ()
val result2 = integral.integrateS ()
val norm = Normal(0.0)(1.0)
val result3 = norm.integral(-1.0, 1.0)
result3
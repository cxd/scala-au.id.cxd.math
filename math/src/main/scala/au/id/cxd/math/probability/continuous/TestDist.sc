import au.id.cxd.math.function.approximate.NumericIntegral
import au.id.cxd.math.probability.continuous.{Beta, Exponential, Gamma, Normal}
import breeze.numerics.pow

import scala.math.{exp, pow}

/**
> dnorm(0,0,1)
[1] 0.3989423

  **/
Normal(0.0)(1.0).pdf(0.0)

/**
> pnorm(0,0,1)
[1] 0.5

  **/
Normal(0)(1).cdf(0)

/**
> pnorm(seq(from=-1.96, to=1.96,by=0.1),0,1)
 [1] 0.02499790 0.03144276 0.03920390 0.04845723 0.05937994 0.07214504
 [7] 0.08691496 0.10383468 0.12302440 0.14457230 0.16852761 0.19489452
[13] 0.22362729 0.25462691 0.28773972 0.32275811 0.35942357 0.39743189
[19] 0.43644054 0.47607782 0.51595344 0.55567000 0.59483487 0.63307174
[25] 0.67003145 0.70540148 0.73891370 0.77035000 0.79954581 0.82639122
[31] 0.85083005 0.87285685 0.89251230 0.90987733 0.92506630 0.93821982
[37] 0.94949742 0.95907049 0.96711588 0.97381016
**/
val q = -1.96 to 1.96 by 0.1
val normal = Normal(0)(1)
q.map(i => normal.cdf(i))
  .foreach(println)

// Gamma Distribution
/**
> dgamma(1,0.5,0.5)
[1] 0.2419707
**/
val gammaD = Gamma(0.5)(0.5)
gammaD.pdf(1)

/**
> pgamma(1,0.5,0.5)
[1] 0.6826895

  **/
gammaD.cdf(1)

/**
  * > dexp(1,0.5)
[1] 0.3032653

  */
val ep = Exponential(0.5)
ep.pdf(1.0)
/**
> pexp(1,0.5)
[1] 0.3934693
**/
ep.cdf(1.0)

/**
> dbeta(0,1,0.3)
[1] 0.3
**/
val b = Beta(1.0, 0.3)
b.pdf(0)

/**
> pbeta(0.5,1,0.3)
[1] 0.1877476
**/
b.cdf(0.5)

/**
> dbeta(0.2,0.5,0.3)
[1] 0.5739667
**/
val b2 = Beta(0.5, 0.3)

b2.pdf(0.2)

/**
  *> pbeta(0.2,0.5,0.3)
[1] 0.2066257

  *
  */
b2.cdf(0.2)

/**
  * Stirlings approximation of the
  * beta function for comparison.
  *
  * @param a
  * @param b
  * @return
  */
def testBeta(a:Double,b:Double) = {
 val a1 = Math.sqrt(2.0*Math.PI)
 val a2 = Math.pow(a, a - 0.5)*Math.pow(b, b - 0.5)
 val b1 = Math.pow(a + b, a + b - 0.5)
 a1 * a2/b1
}

val f = testBeta(0.5,0.3)

val p = b2.pdf(0.2)

p / f

val alpha=0.5
val beta=0.3
val x=0.2
def fn(t:Double) = Math.pow(t,alpha-1.0) * Math.pow(1.0 - t, beta-1.0)

val integral = NumericIntegral(0.0, x, fn).approxIntegral()
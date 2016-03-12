import au.id.cxd.math.definitions.Monad

import Math._

/**
  * a look at investigating
  * sequential functions
  */

// evaluate a function from limit a to limit b
//
type Limit = (Double, Double)

type Integrand[A] = A => A
type Integral[A] = (Int, Double, Limit, Integrand[A])

type NumericIntegral[A] = Integral[A] => A


type IntegralState = (Double, Int, Double, Limit, Integrand[Double])



def run1[A](n:NumericIntegral[A])(input:Integral[A]) : A = n(input)

/**
  * iterative version function for trapezoidal integration
  * @param l
  * @return
  */
def eval(l:Integral[Double]):NumericIntegral[Double] =
  (integral) =>  {
    val (cnt, prev, limit, fn) = integral
    val (a, b) = limit
    if (cnt == 0 || cnt == 1)
      0.5*(b-a)*(fn(a)+fn(b))
    else {
      var len = 1
      for(i <- 1 to cnt-1) {
        len = len << 1
      }
      val delta = (b - a)/len
      var x = a + 0.5 * delta
      var sum = 0.0
      for(i <- 0 to len) {
        sum = sum + fn(x)
        x = x + delta
      }
      sum = sum + 0.5*(prev+(b-a)*sum/len)
      sum
    }
  }

def exampleNorm(mu:Double, sigma:Double)(y:Double) =
  1/(sqrt(2*Math.PI)*sigma) * exp(-(1/(2*pow(sigma,2.0)))*pow(y - mu, 2.0))

val limit = (0.0, 1.0)
val testInt = (5, 0.0, limit, exampleNorm(0,1)_)
val result = run1 (eval (testInt)) (testInt)

exampleNorm(0,1)(0.0)
exampleNorm(0,1)(1.0)






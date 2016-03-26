import au.id.cxd.math.probability
import au.id.cxd.math.probability.analysis.TchebysheffInequality
import au.id.cxd.math.probability.continuous.{Gamma, Normal, Uniform}
//
val norm = Normal(0.0)(1.0)
norm.integral(-1.0, 1.0)
norm.integral(-1.5, 1.5)
norm.integral(-2.0, 2.0)
norm.pdf(0.0)
// TODO work on debugging Gamma distribution
val gamma = Gamma(1)(1)
gamma.integral(0.0, 1.0)
gamma.integral(0.0, 10.0)
//
// > dgamma(c(0,1,5), 1, 1)
// [1] 1.000000000 0.367879441 0.006737947
gamma.pdf(0.0)
gamma.pdf(1.0)
gamma.pdf(5.0)
//
val gamma2 = Gamma(10)(1)
gamma2.integral(0.0, 1.0)
gamma2.integral(0.0, 10.0)
//> dgamma(c(0,1,5), 10, 1)
// [1] 0.000000e+00 1.013777e-06 3.626558e-02
gamma2.pdf(0.0)
gamma2.pdf(1.0)
gamma2.pdf(5.0)
//
val uni = Uniform(0.0)(2.0)
uni.integral(0.5, 1.0)
uni.pdf(0.5)
uni.pdf(1.0)
uni.pdf(0.1)
# Mathematical Functions

Comprehensive guide to special mathematical functions, distance metrics, and statistical utilities.

## Overview

The library provides a wide range of mathematical functions essential for statistical computing:

- **Gamma Functions** - Γ(x), ln Γ(x), digamma, trigamma, incomplete gamma
- **Beta Functions** - B(a,b), ln B(a,b), incomplete beta
- **Error Functions** - erf(x), erfc(x), inverse error functions
- **Distance Metrics** - Euclidean, Manhattan, Cosine, Mahalanobis
- **Correlation & Covariance** - Pearson correlation, covariance matrices
- **Statistical Moments** - Mean, variance, skewness, kurtosis
- **Matrix Operations** - Pseudoinverse, determinants, decompositions
- **Hypergeometric Functions** - Confluent and other hypergeometric functions

**Package**: `au.id.cxd.math.function`

## Quick Start

### Common Mathematical Operations

```scala
import au.id.cxd.math.function.gamma._
import au.id.cxd.math.function.beta._
import au.id.cxd.math.function.distance._
import breeze.linalg._

// Gamma function
val gammaValue = GammaFn(5.0)  // Γ(5) = 24

// Beta function
val betaValue = BetaFn(2.0, 3.0)  // B(2,3)

// Error function
val erfValue = Erf(0.5)

// Euclidean distance
val x = DenseVector(1.0, 2.0, 3.0)
val y = DenseVector(4.0, 5.0, 6.0)
val dist = EuclideanDistance(x, y)

println(f"Gamma(5) = $gammaValue%.2f")
println(f"Distance = $dist%.4f")
```

## Gamma Functions

The gamma function Γ(x) is a generalization of the factorial function.

### Gamma Function

**Package**: `au.id.cxd.math.function.gamma.GammaFn`

For positive integers: $\Gamma(n) = (n-1)!$

General definition: $\Gamma(z) = \int_0^\infty t^{z-1} e^{-t} dt$

```scala
import au.id.cxd.math.function.gamma.GammaFn

// Basic gamma function
val gamma3 = GammaFn(3.0)  // Γ(3) = 2! = 2
val gamma4 = GammaFn(4.0)  // Γ(4) = 3! = 6
val gamma5 = GammaFn(5.0)  // Γ(5) = 4! = 24

println(f"Γ(3.0) = $gamma3%.2f")
println(f"Γ(4.0) = $gamma4%.2f")
println(f"Γ(5.0) = $gamma5%.2f")

// Non-integer values
val gammaHalf = GammaFn(0.5)  // Γ(0.5) = √π
println(f"Γ(0.5) = $gammaHalf%.6f (√π = ${math.sqrt(math.Pi)}%.6f)")

// Larger values
val gamma10 = GammaFn(10.0)  // Γ(10) = 9! = 362880
println(f"Γ(10.0) = $gamma10%.0f")
```

### Log-Gamma Function

**Package**: `au.id.cxd.math.function.gamma.LogGammaFn`

More numerically stable for large arguments:

```scala
import au.id.cxd.math.function.gamma.LogGammaFn

// Log-gamma returns (value, sign)
val (lnGamma, sign) = LogGammaFn(100.0)

println(f"ln(Γ(100)) = $lnGamma%.4f")
println(f"Sign = $sign%.0f")

// Recover gamma from log-gamma
val gamma = sign * math.exp(lnGamma)
println(f"Γ(100) ≈ $gamma%.4e")

// Use log-gamma for large values
val (lnGamma1000, _) = LogGammaFn(1000.0)
println(f"ln(Γ(1000)) = $lnGamma1000%.2f")
```

### Digamma Function (ψ)

**Package**: `au.id.cxd.math.function.gamma.Digamma`

The digamma function is the logarithmic derivative of the gamma function:

$$\psi(x) = \frac{d}{dx} \ln \Gamma(x) = \frac{\Gamma'(x)}{\Gamma(x)}$$

```scala
import au.id.cxd.math.function.gamma.Digamma

val psi1 = Digamma(1.0)
val psi2 = Digamma(2.0)
val psi5 = Digamma(5.0)

println(f"ψ(1) = $psi1%.6f")
println(f"ψ(2) = $psi2%.6f")
println(f"ψ(5) = $psi5%.6f")

// Used in maximum likelihood estimation
// Example: estimating gamma distribution parameters
def gammaLogLikelihood(alpha: Double, observations: Seq[Double]): Double = {
  val n = observations.length
  val sumLogX = observations.map(math.log).sum
  n * alpha * math.log(alpha) - n * LogGammaFn(alpha)._1 + 
    (alpha - 1) * sumLogX
}
```

### Trigamma Function (ψ')

**Package**: `au.id.cxd.math.function.gamma.Trigamma`

The second derivative of ln Γ(x):

$$\psi'(x) = \frac{d^2}{dx^2} \ln \Gamma(x)$$

```scala
import au.id.cxd.math.function.gamma.Trigamma

val triPsi1 = Trigamma(1.0)
val triPsi2 = Trigamma(2.0)

println(f"ψ'(1) = $triPsi1%.6f")
println(f"ψ'(2) = $triPsi2%.6f")

// Used in Newton-Raphson optimization for gamma parameters
```

### Incomplete Gamma Function

**Package**: `au.id.cxd.math.function.gamma.IncompleteGamma`

Lower incomplete gamma: $\gamma(a, x) = \int_0^x t^{a-1} e^{-t} dt$

Upper incomplete gamma: $\Gamma(a, x) = \int_x^\infty t^{a-1} e^{-t} dt$

```scala
import au.id.cxd.math.function.gamma.IncompleteGamma

// Incomplete gamma P(a, x) = γ(a, x) / Γ(a)
val incGamma = IncompleteGamma()
val p = incGamma.gamma_p(2.0, 1.0)
val q = incGamma.gamma_q(2.0, 1.0)  // Complementary: 1 - P(a,x)

println(f"P(2, 1) = $p%.6f")
println(f"Q(2, 1) = $q%.6f")
println(f"P + Q = ${p + q}%.6f")  // Should be 1.0

// Used in chi-squared and gamma distribution CDFs
```

### Inverse Gamma

**Package**: `au.id.cxd.math.function.gamma.InverseGamma`

Find x such that P(a, x) = p:

```scala
import au.id.cxd.math.function.gamma.InverseGamma

// Find x where the incomplete gamma equals 0.5
val invGamma = InverseGamma()
val x = invGamma.op(0.5, 2.0)  // P(2, x) = 0.5

println(f"x where P(2, x) = 0.5: $x%.4f")

// Used in computing quantiles of gamma and chi-squared distributions
```

## Beta Functions

The beta function is related to the gamma function:

$$B(a, b) = \frac{\Gamma(a) \Gamma(b)}{\Gamma(a+b)}$$

### Beta Function

**Package**: `au.id.cxd.math.function.beta.BetaFn`

```scala
import au.id.cxd.math.function.beta.BetaFn

val beta22 = BetaFn(2.0, 2.0)
val beta23 = BetaFn(2.0, 3.0)
val beta35 = BetaFn(3.0, 5.0)

println(f"B(2, 2) = $beta22%.6f")
println(f"B(2, 3) = $beta23%.6f")
println(f"B(3, 5) = $beta35%.6f")

// Relationship to gamma
import au.id.cxd.math.function.gamma.GammaFn
val beta_via_gamma = GammaFn(2.0) * GammaFn(3.0) / GammaFn(5.0)
println(f"Via gamma: $beta_via_gamma%.6f")
```

### Log-Beta Function

**Package**: `au.id.cxd.math.function.beta.LogBetaFn`

```scala
import au.id.cxd.math.function.beta.LogBetaFn

val lnBeta = LogBetaFn(10.0, 20.0)
println(f"ln(B(10, 20)) = $lnBeta%.6f")

// More stable for large parameters
val lnBetaLarge = LogBetaFn(100.0, 200.0)
val betaLarge = math.exp(lnBetaLarge)
println(f"B(100, 200) ≈ $betaLarge%.4e")
```

### Incomplete Beta Function

**Package**: `au.id.cxd.math.function.beta.IncompleteBetaFn`

$$I_x(a, b) = \frac{1}{B(a,b)} \int_0^x t^{a-1}(1-t)^{b-1} dt$$

```scala
import au.id.cxd.math.function.beta.IncompleteBetaFn

// Regularized incomplete beta function
val incBeta = IncompleteBetaFn()
val ix = incBeta.op(0.5, 2.0, 3.0)  // I_0.5(2, 3)

println(f"I_0.5(2, 3) = $ix%.6f")

// Used in beta distribution CDF
// Also used in binomial and F-distribution CDFs
```

### Inverse Beta

**Package**: `au.id.cxd.math.function.beta.InverseBeta`

Find x such that I_x(a, b) = p:

```scala
import au.id.cxd.math.function.beta.InverseBeta

// Find x where incomplete beta equals 0.5
val invBeta = InverseBeta()
val x = invBeta.op(0.5, 2.0, 3.0)  // I_x(2, 3) = 0.5

println(f"x where I_x(2, 3) = 0.5: $x%.6f")

// Used in computing beta distribution quantiles
```

## Error Functions

Error functions are related to the normal distribution.

### Error Function (erf)

**Package**: `au.id.cxd.math.function.gamma.Erf`

$$\text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt$$

```scala
import au.id.cxd.math.function.gamma.Erf

val erf0 = Erf(0.0)  // erf(0) = 0
val erf1 = Erf(1.0)
val erf2 = Erf(2.0)

println(f"erf(0) = $erf0%.6f")
println(f"erf(1) = $erf1%.6f")
println(f"erf(2) = $erf2%.6f")

// Relationship to normal CDF
import au.id.cxd.math.probability.continuous.Normal
val normal = Normal(0, 1)
val cdfAt1 = normal.cdf(1.0)
val erfAt1 = 0.5 * (1.0 + Erf(1.0 / math.sqrt(2.0)))

println(f"Normal CDF(1): $cdfAt1%.6f")
println(f"Via erf: $erfAt1%.6f")
```

### Complementary Error Function (erfc)

**Package**: `au.id.cxd.math.function.gamma.Erfc`

$$\text{erfc}(x) = 1 - \text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_x^\infty e^{-t^2} dt$$

```scala
import au.id.cxd.math.function.gamma.{Erf, Erfc}

val x = 1.5
val erf_x = Erf(x)
val erfc_x = Erfc(x)

println(f"erf($x) = $erf_x%.6f")
println(f"erfc($x) = $erfc_x%.6f")
println(f"erf + erfc = ${erf_x + erfc_x}%.6f")  // Should be 1.0

// More accurate for large x
val large_x = 5.0
val erfc_large = Erfc(large_x)
println(f"erfc($large_x) = $erfc_large%.10e")
```

### Inverse Error Functions

**Package**: `au.id.cxd.math.function.gamma.InvErf` and `InvErfc`

```scala
import au.id.cxd.math.function.gamma.{InvErf, InvErfc}

// Find x such that erf(x) = 0.5
val invErf = InvErf()
val x1 = invErf.op(0.5)

println(f"x where erf(x) = 0.5: $x1%.6f")
println(f"Verification: erf($x1) = ${Erf(x1)}%.6f")

// Inverse complementary error function
val invErfc = InvErfc()
val x2 = invErfc.op(0.5)

println(f"x where erfc(x) = 0.5: $x2%.6f")
```

## Distance Metrics

Measure similarity or dissimilarity between vectors.

### Euclidean Distance

**Package**: `au.id.cxd.math.function.distance.EuclideanDistance`

$$d(x, y) = \sqrt{\sum_{i=1}^n (x_i - y_i)^2}$$

```scala
import au.id.cxd.math.function.distance.EuclideanDistance
import breeze.linalg._

val x = DenseVector(1.0, 2.0, 3.0)
val y = DenseVector(4.0, 5.0, 6.0)

// Squared Euclidean distance
val distSq = EuclideanDistance(x, y)
val dist = math.sqrt(distSq)

println(f"Squared distance: $distSq%.4f")
println(f"Euclidean distance: $dist%.4f")

// Using with matrices
val points = DenseMatrix(
  (1.0, 2.0),
  (4.0, 5.0),
  (7.0, 8.0)
)

// Distance between first two points
val point1 = points(0, ::).t
val point2 = points(1, ::).t
val d = math.sqrt(EuclideanDistance(point1, point2))
println(f"Distance: $d%.4f")
```

### Cosine Distance

**Package**: `au.id.cxd.math.function.distance.CosineDistance`

Measures the angle between vectors:

$$\text{similarity} = \frac{x \cdot y}{||x|| \times ||y||}$$

$$\text{distance} = 1 - \text{similarity}$$

```scala
import au.id.cxd.math.function.distance.CosineDistance
import breeze.linalg._

val x = DenseVector(1.0, 2.0, 3.0)
val y = DenseVector(2.0, 4.0, 6.0)  // Parallel to x

val cosine = CosineDistance()
val distance = cosine.measure(x, y)
val similarity = 1.0 - distance

println(f"Cosine distance: $distance%.6f")
println(f"Cosine similarity: $similarity%.6f")

// For parallel vectors, similarity = 1, distance = 0
println(f"Parallel vectors: similarity = $similarity%.6f")

// Orthogonal vectors
val v1 = DenseVector(1.0, 0.0, 0.0)
val v2 = DenseVector(0.0, 1.0, 0.0)
val dist_ortho = cosine.measure(v1, v2)
println(f"Orthogonal vectors distance: $dist_ortho%.6f")  // Should be 1.0
```

### Mahalanobis Distance

**Package**: `au.id.cxd.math.function.distance.MahalanobisDistance`

Distance that accounts for correlations between variables:

$$d(x, y) = \sqrt{(x-y)^T \Sigma^{-1} (x-y)}$$

```scala
import au.id.cxd.math.function.distance.MahalanobisDistance
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 4.0),
  (4.0, 5.0)
)

val x = DenseVector(2.5, 3.5)
val y = DenseVector(3.5, 4.5)

// Mahalanobis distance using data covariance
val mahal = MahalanobisDistance(data)
val distance = mahal.measure(x, y)

println(f"Mahalanobis distance: $distance%.6f")

// Takes into account the covariance structure of the data
// Useful for detecting outliers and classification
```

## Correlation and Covariance

### Correlation Matrix

**Package**: `au.id.cxd.math.function.distance.Cor`

```scala
import au.id.cxd.math.function.distance.Cor
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 2.0, 3.0),
  (2.0, 4.0, 6.0),
  (3.0, 6.0, 9.0),
  (4.0, 8.0, 12.0)
)

// Correlation matrix
val corMatrix = Cor(data)

println("Correlation matrix:")
println(corMatrix)

// Diagonal should be all 1s
// Off-diagonal shows correlation between variables
```

### Covariance Matrix

**Package**: `au.id.cxd.math.function.distance.Cov`

```scala
import au.id.cxd.math.function.distance.Cov
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 4.0),
  (4.0, 5.0)
)

// Covariance matrix
val cov = new Cov(data, data)
val covMatrix = cov.op()

println("Covariance matrix:")
println(covMatrix)

// Measures how variables vary together
```

## Statistical Moments

Functions for computing statistical moments.

### Mean

**Package**: `au.id.cxd.math.function.moments.Mean`

```scala
import au.id.cxd.math.function.moments.Mean
import breeze.linalg._

val data = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)

val mean = Mean(data)
println(f"Mean: $mean%.2f")

// For matrices (column-wise mean)
val matrix = DenseMatrix(
  (1.0, 2.0),
  (3.0, 4.0),
  (5.0, 6.0)
)

val colMeans = Mean(matrix)
println(s"Column means: $colMeans")
```

### Variance

**Package**: `au.id.cxd.math.function.moments.Variance` and `VarianceUnbiased`

```scala
import au.id.cxd.math.function.moments.{Variance, VarianceUnbiased}
import breeze.linalg._

val data = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)

// Biased variance (divides by n)
val varBiased = Variance(data)
println(f"Biased variance: $varBiased%.4f")

// Unbiased variance (divides by n-1)
val varUnbiased = VarianceUnbiased(data)
println(f"Unbiased variance: $varUnbiased%.4f")

// Standard deviation
val std = math.sqrt(varUnbiased)
println(f"Standard deviation: $std%.4f")
```

### Skewness

**Package**: `au.id.cxd.math.function.moments.Skewness`

Measure of asymmetry:

```scala
import au.id.cxd.math.function.moments.Skewness
import breeze.linalg._

// Right-skewed data
val rightSkewed = DenseVector(1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 10.0)
val skew1 = Skewness(rightSkewed)

// Left-skewed data
val leftSkewed = DenseVector(1.0, 8.0, 8.0, 9.0, 9.0, 9.0, 10.0)
val skew2 = Skewness(leftSkewed)

// Symmetric data
val symmetric = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)
val skew3 = Skewness(symmetric)

println(f"Right-skewed: $skew1%.4f (positive)")
println(f"Left-skewed: $skew2%.4f (negative)")
println(f"Symmetric: $skew3%.4f (near zero)")
```

### Kurtosis

**Package**: `au.id.cxd.math.function.moments.Kurtosis`

Measure of tail heaviness:

```scala
import au.id.cxd.math.function.moments.Kurtosis
import breeze.linalg._

val data = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
val kurt = Kurtosis(data)

println(f"Kurtosis: $kurt%.4f")

// Normal distribution has kurtosis ≈ 3 (excess kurtosis = 0)
// Higher values indicate heavier tails
// Lower values indicate lighter tails
```

## Matrix Operations

### Pseudoinverse

**Package**: `au.id.cxd.math.function.matrix.PseudoInverse`

Moore-Penrose pseudoinverse for solving least squares:

```scala
import au.id.cxd.math.function.matrix.PseudoInverse
import breeze.linalg._

val A = DenseMatrix(
  (1.0, 2.0),
  (3.0, 4.0),
  (5.0, 6.0)
)

// Compute pseudoinverse
val Aplus = PseudoInverse(A)

println("Original matrix A:")
println(A)
println("\nPseudoinverse A+:")
println(Aplus)

// Verify: A * A+ * A = A
val verification = A * Aplus * A
println("\nVerification (A * A+ * A):")
println(verification)

// Use for least squares: x = A+ * b
val b = DenseVector(1.0, 2.0, 3.0)
val x = Aplus * b
println(s"\nLeast squares solution: $x")
```

### Log Multiplication (for numerical stability)

**Package**: `au.id.cxd.math.function.matrix.LogMult`

```scala
import au.id.cxd.math.function.matrix.LogMult

// When dealing with very small probabilities
val logProb1 = -100.0  // log(p1)
val logProb2 = -200.0  // log(p2)

// Instead of: p1 * p2 (which would underflow)
// Use: exp(log(p1) + log(p2))
val logProduct = LogMult(logProb1, logProb2)
val product = math.exp(logProduct)

println(f"log(p1 * p2) = $logProduct%.2f")
println(f"p1 * p2 ≈ $product%.4e")
```

## Hypergeometric Functions

Advanced special functions for specific distributions.

### Confluent Hypergeometric

Used in distributions like the non-central chi-squared:

```scala
import au.id.cxd.math.function.hypergeometric._

// Confluent hypergeometric function 1F1(a; b; x)
// Used in computing non-central distributions
// Available but typically used internally by probability distributions
```

## Constants

**Package**: `au.id.cxd.math.function.Constants`

```scala
import au.id.cxd.math.function.Constants

// Mathematical constants
val pi = Constants.PI
val e = Constants.E
val sqrt2 = Constants.SQRT2
val sqrt2pi = Constants.SQRT_2PI

println(f"π = $pi%.10f")
println(f"e = $e%.10f")
println(f"√2 = $sqrt2%.10f")
println(f"√(2π) = $sqrt2pi%.10f")
```

## Polynomial and Series Functions

### Polynomial Evaluation

**Package**: `au.id.cxd.math.function.series.PolyVal`

```scala
import au.id.cxd.math.function.series.PolyVal

// Evaluate polynomial: a0 + a1*x + a2*x^2 + a3*x^3
val coeffs = Array(1.0, 2.0, 3.0, 4.0)  // 1 + 2x + 3x^2 + 4x^3
val x = 2.0

val result = PolyVal(coeffs, x)
println(f"P($x) = $result%.2f")  // 1 + 4 + 12 + 32 = 49

// Verify
val manual = 1.0 + 2.0*x + 3.0*x*x + 4.0*x*x*x
println(f"Manual calculation: $manual%.2f")
```

### Polynomial Expansion

**Package**: `au.id.cxd.math.function.series.PolynomialExpansion`

Generate polynomial features:

```scala
import au.id.cxd.math.function.series.PolynomialExpansion
import breeze.linalg._

val x = DenseVector(2.0, 3.0)

// Generate up to degree 2: [1, x1, x2, x1^2, x1*x2, x2^2]
val polyFeatures = PolynomialExpansion(x, degree = 2)

println("Polynomial features:")
println(polyFeatures)

// Useful for polynomial regression
```

## Complete Examples

### Example 1: Computing Distribution Parameters

```scala
import au.id.cxd.math.function.gamma._
import au.id.cxd.math.function.moments._
import breeze.linalg._

// Estimate gamma distribution parameters from data
val data = DenseVector(1.2, 2.3, 1.8, 2.9, 1.5, 2.1, 1.9, 2.7)

val mean = Mean(data)
val variance = VarianceUnbiased(data)

// Method of moments estimation for gamma distribution
// mean = shape * scale
// variance = shape * scale^2
val scale = variance / mean
val shape = mean / scale

println(f"Estimated shape (α): $shape%.4f")
println(f"Estimated scale (θ): $scale%.4f")

// Verify using gamma function
val gammaValue = GammaFn(shape)
val expectedMean = shape * scale
println(f"Expected mean: $expectedMean%.4f (observed: $mean%.4f)")
```

### Example 2: Multivariate Distance Analysis

```scala
import au.id.cxd.math.function.distance._
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 2.0, 3.0),
  (2.0, 3.0, 4.0),
  (3.0, 4.0, 5.0),
  (10.0, 11.0, 12.0)  // Potential outlier
)

// Compute pairwise distances
val n = data.rows
println("Euclidean distances from first point:")
val point0 = data(0, ::).t

for (i <- 1 until n) {
  val pointi = data(i, ::).t
  val dist = math.sqrt(EuclideanDistance(point0, pointi))
  println(f"  Distance to point $i: $dist%.4f")
}

// Compute Mahalanobis distances (accounts for correlations)
val mahal = MahalanobisDistance(data)
println("\nMahalanobis distances from mean:")

val mean = breeze.linalg.sum(data(::, *)).t / n.toDouble
for (i <- 0 until n) {
  val pointi = data(i, ::).t
  val dist = mahal.measure(pointi, mean)
  println(f"  Point $i: $dist%.4f")
}
```

### Example 3: Correlation Analysis

```scala
import au.id.cxd.math.function.distance.Cor
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 2.0, 3.0),
  (2.0, 4.0, 5.0),
  (3.0, 6.0, 7.0),
  (4.0, 8.0, 9.0)
)

// Compute correlation matrix
val corMatrix = Cor(data)

println("Correlation matrix:")
println(corMatrix)

// Interpret correlations
val n = corMatrix.rows
for (i <- 0 until n; j <- i + 1 until n) {
  val corr = corMatrix(i, j)
  val strength = if (math.abs(corr) > 0.9) "very strong"
                else if (math.abs(corr) > 0.7) "strong"
                else if (math.abs(corr) > 0.5) "moderate"
                else "weak"
  
  println(f"Variables $i and $j: r = $corr%.4f ($strength)")
}
```

### Example 4: Distribution Quantiles

```scala
import au.id.cxd.math.function.gamma._
import au.id.cxd.math.function.beta._

// Compute quantiles using inverse functions

// Normal distribution via inverse error function
val p = 0.975  // 97.5th percentile
val invErf = InvErf()
val z = math.sqrt(2.0) * invErf.op(2.0 * p - 1.0)
println(f"Normal z-score for p=$p: $z%.4f")  // Should be ≈ 1.96

// Beta distribution quantile
val invBeta = InverseBeta()
val betaQuantile = invBeta.op(0.5, 2.0, 5.0)  // Median of Beta(2,5)
println(f"Beta(2,5) median: $betaQuantile%.4f")

// Gamma distribution quantile
val invGamma = InverseGamma()
val gammaQuantile = invGamma.op(0.95, 5.0)  // 95th percentile
println(f"Gamma(5) 95th percentile: $gammaQuantile%.4f")
```

## Best Practices

### Numerical Stability

1. **Use log versions** for large values:
   - `LogGammaFn` instead of `GammaFn` for large arguments
   - `LogBetaFn` instead of `BetaFn` for large parameters

2. **Use complementary functions** for extreme values:
   - `Erfc` for large x instead of `1 - Erf(x)`
   - Complementary incomplete gamma for upper tail

3. **Watch for underflow**:
   - Use log-space arithmetic when dealing with very small probabilities
   - `LogMult` for stable multiplication

### Choosing Distance Metrics

| Metric | Best For | Properties |
|--------|----------|------------|
| **Euclidean** | Raw measurements | Scale-sensitive, intuitive |
| **Cosine** | Direction comparison | Scale-invariant, good for text |
| **Mahalanobis** | Correlated features | Accounts for covariance |

### Performance Tips

```scala
// For repeated calculations, store intermediate results
val lnGammaResults = (1 to 100).map { i =>
  val (lnG, sign) = LogGammaFn(i.toDouble)
  (i, lnG, sign)
}.toMap

// Reuse distance metric objects
val cosine = CosineDistance()
val distances = points.map(p => cosine.measure(query, p))

// For correlation matrices, normalize once
import au.id.cxd.math.function.transform.StandardisedNormalisation
val normalizer = StandardisedNormalisation()
val normalized = normalizer.transform(data)
val corMatrix = Cor(normalized)  // Already normalized
```

## Common Pitfalls

- ❌ Using `GammaFn` for very large arguments (use `LogGammaFn`)
- ❌ Computing `1 - Erf(x)` for large x (use `Erfc`)
- ❌ Ignoring numerical precision for iterative algorithms
- ❌ Using Euclidean distance on unnormalized features
- ❌ Forgetting that distance metrics may return squared values
- ❌ Not checking for convergence in inverse functions

## See Also

- [Probability Distributions](Probability-Distributions.md) - Using these functions in distributions
- [Statistical Tests](Statistical-Tests.md) - Tests using special functions
- [Data Processing](Data-Processing.md) - Preprocessing before distance calculations
- [Multivariate Analysis](Multivariate-Analysis.md) - Covariance and correlation in PCA
- [API Quick Reference](API-Quick-Reference.md) - Quick syntax lookup

---

[← Back to Home](Home.md)

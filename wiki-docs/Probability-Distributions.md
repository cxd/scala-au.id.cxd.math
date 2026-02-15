# Probability Distributions

The library provides a comprehensive collection of probability distributions, both continuous and discrete, with support for PDF, CDF, quantile functions, and random number generation.

## Overview

All distributions implement the `Distribution` trait, which provides:
- `pdf(x)` - Probability density/mass function
- `cdf(x)` - Cumulative distribution function
- `invcdf(p)` - Inverse CDF (quantile function)
- `draw(n)` - Generate n random samples

## Continuous Distributions

### Normal (Gaussian) Distribution

**Package**: `au.id.cxd.math.probability.continuous.Normal`

```scala
import au.id.cxd.math.probability.continuous.Normal

// Create standard normal N(0, 1)
val standard = Normal(mu = 0.0, sigma = 1.0)

// Create custom normal N(5, 2)
val custom = Normal(mu = 5.0, sigma = 2.0)

// Calculate probabilities
val pdf = standard.pdf(1.5)      // P(X = 1.5)
val cdf = standard.cdf(1.5)      // P(X ≤ 1.5)
val quantile = standard.invcdf(0.95)  // 95th percentile

// Generate samples
val samples = standard.draw(1000)
```

### Uniform Distribution

**Package**: `au.id.cxd.math.probability.continuous.Uniform`

```scala
import au.id.cxd.math.probability.continuous.Uniform

val uniform = Uniform(a = 0.0, b = 1.0)
val prob = uniform.pdf(0.5)
val samples = uniform.draw(100)
```

### Exponential Distribution

**Package**: `au.id.cxd.math.probability.continuous.Exponential`

```scala
import au.id.cxd.math.probability.continuous.Exponential

val exponential = Exponential(lambda = 1.5)
val prob = exponential.pdf(2.0)
val cdf = exponential.cdf(2.0)
```

### Gamma Distribution

**Package**: `au.id.cxd.math.probability.continuous.Gamma`

```scala
import au.id.cxd.math.probability.continuous.Gamma

val gamma = Gamma(shape = 2.0, scale = 1.0)
val prob = gamma.pdf(3.0)
val samples = gamma.draw(500)
```

### Beta Distribution

**Package**: `au.id.cxd.math.probability.continuous.Beta`

```scala
import au.id.cxd.math.probability.continuous.Beta

val beta = Beta(alpha = 2.0, beta = 5.0)
val prob = beta.pdf(0.3)
val mean = beta.mean()
val variance = beta.variance()
```

### Chi-Square Distribution

**Package**: `au.id.cxd.math.probability.continuous.ChiSquare`

```scala
import au.id.cxd.math.probability.continuous.ChiSquare

val chiSq = ChiSquare(degreesOfFreedom = 10)
val prob = chiSq.pdf(5.0)
val cdf = chiSq.cdf(5.0)
```

### Student's t-Distribution

**Package**: `au.id.cxd.math.probability.continuous.StudentT`

```scala
import au.id.cxd.math.probability.continuous.StudentT

val t = StudentT(degreesOfFreedom = 10)
val prob = t.pdf(2.0)
val criticalValue = t.invcdf(0.975)  // Two-tailed 95% confidence
```

### F-Distribution

**Package**: `au.id.cxd.math.probability.continuous.FDistribution`

```scala
import au.id.cxd.math.probability.continuous.FDistribution

val f = FDistribution(df1 = 5, df2 = 10)
val prob = f.pdf(2.5)
val cdf = f.cdf(2.5)
```

### Log-Normal Distribution

**Package**: `au.id.cxd.math.probability.continuous.LogNormal`

```scala
import au.id.cxd.math.probability.continuous.LogNormal

val logNormal = LogNormal(mu = 0.0, sigma = 1.0)
val prob = logNormal.pdf(2.0)
val samples = logNormal.draw(100)
```

### Gumbel Distribution

**Package**: `au.id.cxd.math.probability.continuous.Gumbel`

```scala
import au.id.cxd.math.probability.continuous.Gumbel

val gumbel = Gumbel(mu = 0.0, beta = 1.0)
val prob = gumbel.pdf(1.0)
```

## Discrete Distributions

### Binomial Distribution

**Package**: `au.id.cxd.math.probability.discrete.Binomial`

```scala
import au.id.cxd.math.probability.discrete.Binomial

// n trials with probability p
val binomial = Binomial(n = 10, p = 0.5)

// Probability of exactly k successes
val probExact = binomial.pdf(5)  // P(X = 5)

// Probability of at most k successes
val probCumulative = binomial.cdf(5)  // P(X ≤ 5)

// Generate random samples
val samples = binomial.draw(100)
```

### Poisson Distribution

**Package**: `au.id.cxd.math.probability.discrete.Poisson`

```scala
import au.id.cxd.math.probability.discrete.Poisson

val poisson = Poisson(lambda = 3.5)
val prob = poisson.pdf(4)  // P(X = 4)
val cdf = poisson.cdf(4)   // P(X ≤ 4)
```

### Geometric Distribution

**Package**: `au.id.cxd.math.probability.discrete.Geometric`

```scala
import au.id.cxd.math.probability.discrete.Geometric

val geometric = Geometric(p = 0.3)
val prob = geometric.pdf(5)  // Probability of first success on trial 5
```

### Negative Binomial Distribution

**Package**: `au.id.cxd.math.probability.discrete.NegativeBinomial`

```scala
import au.id.cxd.math.probability.discrete.NegativeBinomial

val negBinomial = NegativeBinomial(r = 5, p = 0.5)
val prob = negBinomial.pdf(10)
```

### Hypergeometric Distribution

**Package**: `au.id.cxd.math.probability.discrete.HyperGeometric`

```scala
import au.id.cxd.math.probability.discrete.HyperGeometric

// Population N, success states K, sample size n
val hypergeom = HyperGeometric(N = 50, K = 10, n = 5)
val prob = hypergeom.pdf(2)  // P(X = 2 successes in sample)
```

## Multivariate Distributions

### Multivariate Normal Distribution

**Package**: `au.id.cxd.math.probability.continuous.MultivariateNormal`

```scala
import au.id.cxd.math.probability.continuous.MultivariateNormal
import breeze.linalg._

// Mean vector and covariance matrix
val mu = DenseVector(0.0, 0.0)
val sigma = DenseMatrix((1.0, 0.5), (0.5, 1.0))

val mvn = MultivariateNormal(mu, sigma)

// Evaluate PDF at a point
val x = DenseVector(1.0, 1.0)
val prob = mvn.pdf(x)

// Generate samples
val samples = mvn.draw(100)  // Returns DenseMatrix with 100 rows
```

## Working with Random Deviates

All distributions support random number generation through the `RandomDeviate` trait:

```scala
import au.id.cxd.math.probability.random.RNormal
import au.id.cxd.math.probability.random.RBinomial

// Generate random normals
val rNormal = RNormal(mu = 0.0, sigma = 1.0)
val normalSample = rNormal.draw()
val normalSamples = rNormal.draw(1000)

// Generate random binomials
val rBinomial = RBinomial(n = 10, p = 0.5)
val binomialSample = rBinomial.draw()
```

## Distribution Properties

Most distributions provide methods for computing moments:

```scala
val normal = Normal(mu = 5.0, sigma = 2.0)

// Basic properties
val mean = normal.mean()
val variance = normal.variance()
val stdDev = normal.stddev()

// Higher moments (where available)
val skewness = normal.skewness()
val kurtosis = normal.kurtosis()
```

## Probability Calculations

### Survival Function

The survival function (1 - CDF) can be computed:

```scala
val normal = Normal(0.0, 1.0)
val survival = 1.0 - normal.cdf(1.96)  // P(X > 1.96)
```

### Confidence Intervals

Use the inverse CDF for confidence intervals:

```scala
val normal = Normal(0.0, 1.0)
val lower = normal.invcdf(0.025)   // 2.5th percentile
val upper = normal.invcdf(0.975)   // 97.5th percentile
// [lower, upper] is the 95% confidence interval
```

## Best Practices

1. **Reuse distribution objects** when possible - they're lightweight
2. **Use appropriate distributions** for your data type
3. **Validate parameters** before creating distributions
4. **Check for numerical stability** with extreme parameter values
5. **Use RandomDeviate classes** for efficient bulk sampling

## Common Patterns

### Hypothesis Testing

```scala
import au.id.cxd.math.probability.continuous.{Normal, StudentT}

// Z-test
val z = (sampleMean - mu0) / (sigma / math.sqrt(n))
val pValue = 2 * (1 - Normal(0, 1).cdf(math.abs(z)))

// t-test
val t = (sampleMean - mu0) / (sampleStdDev / math.sqrt(n))
val tDist = StudentT(n - 1)
val pValueT = 2 * (1 - tDist.cdf(math.abs(t)))
```

### Bayesian Inference

```scala
import au.id.cxd.math.probability.continuous.{Normal, Beta}

// Beta-Binomial conjugate prior
val prior = Beta(alpha = 1, beta = 1)  // Uniform prior
val successes = 7
val trials = 10
val posterior = Beta(alpha = 1 + successes, beta = 1 + (trials - successes))

val posteriorMean = posterior.mean()
```

## See Also

- [Statistical Tests](Statistical-Tests.md) - Using distributions for hypothesis testing
- [Regression Methods](Regression-Methods.md) - Distributions in regression models
- [API Quick Reference](API-Quick-Reference.md) - Quick lookup of common operations
- [Examples Catalog](Examples-Catalog.md) - Working code examples

---

[← Back to Home](Home.md)

# Statistical Tests

This library provides a variety of statistical hypothesis tests for inference and analysis.

## Overview

Statistical tests in this library follow a consistent pattern:
1. Prepare your data
2. Create the test object
3. Examine the test statistic and p-value
4. Interpret the results

## Univariate Tests

### ANOVA (Analysis of Variance)

Test if means of multiple groups are equal.

**Package**: `au.id.cxd.math.function.anova.Anova`

```scala
import au.id.cxd.math.function.anova.Anova
import breeze.linalg._

// Data for 3 groups
val group1 = DenseVector(23.0, 25.0, 27.0, 24.0, 26.0)
val group2 = DenseVector(30.0, 32.0, 29.0, 31.0, 33.0)
val group3 = DenseVector(18.0, 20.0, 19.0, 21.0, 17.0)

// Combine into matrix (each column is a group)
val data = DenseMatrix.horzcat(
  group1.asDenseMatrix.t,
  group2.asDenseMatrix.t,
  group3.asDenseMatrix.t
)

// Perform ANOVA
val anova = Anova(data)

println(s"F-statistic: ${anova.fStatistic}")
println(s"p-value: ${anova.pValue}")
println(s"Between-group variance: ${anova.betweenGroupVariance}")
println(s"Within-group variance: ${anova.withinGroupVariance}")

// Interpret
if (anova.pValue < 0.05) {
  println("Reject null hypothesis: means are significantly different")
} else {
  println("Fail to reject null hypothesis: means are not significantly different")
}
```

## Multivariate Tests

### MANOVA (Multivariate Analysis of Variance)

Test if mean vectors of multiple groups are equal across multiple dependent variables.

**Package**: `au.id.cxd.math.function.anova.Manova`

```scala
import au.id.cxd.math.function.anova.Manova
import breeze.linalg._

// Multivariate data: each row is an observation, columns are variables
val group1 = DenseMatrix(
  (5.0, 3.0),
  (6.0, 4.0),
  (7.0, 5.0)
)
val group2 = DenseMatrix(
  (8.0, 6.0),
  (9.0, 7.0),
  (10.0, 8.0)
)

// Create list of groups
val groups = Seq(group1, group2)

// Perform MANOVA
val manova = Manova(groups)

println(s"Wilks' Lambda: ${manova.wilksLambda}")
println(s"F-statistic: ${manova.fStatistic}")
println(s"p-value: ${manova.pValue}")
println(s"Degrees of freedom: ${manova.df1}, ${manova.df2}")
```

## Normality Tests

### Shapiro-Wilk Test

Test if data comes from a normal distribution. Works well for small to medium sample sizes (n < 5000).

**Package**: `au.id.cxd.math.function.approximate.ShapiroWilksTest`

```scala
import au.id.cxd.math.function.approximate.ShapiroWilksTest
import breeze.linalg._

val data = DenseVector(2.3, 2.5, 2.7, 2.4, 2.6, 2.8, 2.2, 2.9)

val test = ShapiroWilksTest(data)

println(s"W statistic: ${test.W}")
println(s"p-value: ${test.pValue}")

if (test.pValue > 0.05) {
  println("Data appears to be normally distributed")
} else {
  println("Data does not appear to be normally distributed")
}
```

### Jarque-Bera Test

Test for normality based on sample skewness and kurtosis.

**Package**: `au.id.cxd.math.function.approximate.JarqueBeraTest`

```scala
import au.id.cxd.math.function.approximate.JarqueBeraTest
import breeze.linalg._

val data = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)

val test = JarqueBeraTest(data)

println(s"JB statistic: ${test.statistic}")
println(s"p-value: ${test.pValue}")
println(s"Skewness: ${test.skewness}")
println(s"Kurtosis: ${test.kurtosis}")
```

### Anderson-Darling Test

Another normality test, particularly sensitive to deviations in the tails.

**Package**: `au.id.cxd.math.function.approximate.AndersonDarlingTest`

```scala
import au.id.cxd.math.function.approximate.AndersonDarlingTest
import breeze.linalg._

val data = DenseVector(2.1, 2.3, 2.5, 2.7, 2.9, 3.1, 3.3)

val test = AndersonDarlingTest(data)

println(s"A² statistic: ${test.statistic}")
println(s"Critical values: ${test.criticalValues}")
println(s"Significance levels: ${test.significanceLevels}")
```

## Multivariate Normality Tests

### Mardia Test

Test for multivariate normality using skewness and kurtosis.

**Package**: `au.id.cxd.math.function.approximate.MardiaTest`

```scala
import au.id.cxd.math.function.approximate.MardiaTest
import breeze.linalg._

// Multivariate data (n observations x p variables)
val data = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 4.0),
  (4.0, 5.0),
  (5.0, 6.0)
)

val test = MardiaTest(data)

println(s"Skewness statistic: ${test.skewnessStatistic}")
println(s"Skewness p-value: ${test.skewnessPValue}")
println(s"Kurtosis statistic: ${test.kurtosisStatistic}")
println(s"Kurtosis p-value: ${test.kurtosisPValue}")

if (test.skewnessPValue > 0.05 && test.kurtosisPValue > 0.05) {
  println("Data appears to be multivariate normal")
}
```

### Henze-Zirkler Test

Another test for multivariate normality, based on a distance measure.

**Package**: `au.id.cxd.math.function.approximate.HenzeZirklerTest`

```scala
import au.id.cxd.math.function.approximate.HenzeZirklerTest
import breeze.linalg._

val data = DenseMatrix.rand[Double](50, 3)  // 50 observations, 3 variables

val test = HenzeZirklerTest(data)

println(s"HZ statistic: ${test.statistic}")
println(s"p-value: ${test.pValue}")
```

## Goodness-of-Fit Tests

### Chi-Square Goodness-of-Fit

Test if observed frequencies match expected frequencies.

```scala
import au.id.cxd.math.probability.continuous.ChiSquare
import breeze.linalg._

// Observed frequencies
val observed = DenseVector(18.0, 22.0, 25.0, 35.0)

// Expected frequencies (uniform distribution)
val total = sum(observed)
val expected = DenseVector.fill(observed.length)(total / observed.length)

// Calculate chi-square statistic
val chiSq = sum((observed - expected).map(x => x * x) / expected)

// Degrees of freedom
val df = observed.length - 1

// Get p-value
val chiDist = ChiSquare(df)
val pValue = 1.0 - chiDist.cdf(chiSq)

println(f"Chi-square statistic: $chiSq%.3f")
println(f"Degrees of freedom: $df")
println(f"p-value: $pValue%.4f")
```

### Kolmogorov-Smirnov Test

Test if sample comes from a specific distribution (comparing CDFs).

```scala
import au.id.cxd.math.probability.continuous.Normal
import breeze.linalg._
import breeze.stats._

val data = DenseVector.rand(100).map(_ * 10 + 50)  // Some sample data

// Test against normal distribution
val mu = mean(data)
val sigma = stddev(data)
val normal = Normal(mu, sigma)

// Calculate D statistic (maximum difference between empirical and theoretical CDF)
val sortedData = data.toArray.sorted
val empiricalCDF = (1 to sortedData.length).map(_.toDouble / sortedData.length)
val theoreticalCDF = sortedData.map(normal.cdf)

val D = (empiricalCDF zip theoreticalCDF).map { case (e, t) => math.abs(e - t) }.max

println(f"KS statistic D: $D%.4f")

// Critical value for alpha = 0.05 (approximate)
val n = data.length
val criticalValue = 1.36 / math.sqrt(n)

if (D < criticalValue) {
  println("Data appears to follow the specified normal distribution")
} else {
  println("Data does not appear to follow the specified normal distribution")
}
```

## Correlation Tests

### Testing for Correlation

```scala
import breeze.linalg._
import breeze.stats._
import au.id.cxd.math.probability.continuous.StudentT

// Two variables
val x = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
val y = DenseVector(2.0, 4.0, 5.0, 4.0, 5.0, 7.0, 8.0, 9.0)

// Calculate correlation coefficient
val r = breeze.stats.corrcoeff(x, y)

// Test if correlation is significant
val n = x.length
val t = r * math.sqrt(n - 2) / math.sqrt(1 - r * r)
val tDist = StudentT(n - 2)
val pValue = 2 * (1 - tDist.cdf(math.abs(t)))

println(f"Correlation coefficient: $r%.3f")
println(f"t-statistic: $t%.3f")
println(f"p-value: $pValue%.4f")

if (pValue < 0.05) {
  println("Correlation is statistically significant")
}
```

## Multiple Comparison Corrections

When performing multiple tests, adjust p-values to control for false positives:

### Bonferroni Correction

```scala
val pValues = Seq(0.01, 0.03, 0.04, 0.06, 0.08)
val alpha = 0.05
val numTests = pValues.length

// Bonferroni-corrected alpha
val correctedAlpha = alpha / numTests

val significant = pValues.filter(_ < correctedAlpha)
println(s"Significant tests (Bonferroni): ${significant.length} out of $numTests")
```

### Benjamini-Hochberg (FDR) Correction

```scala
val pValues = Seq(0.01, 0.03, 0.04, 0.06, 0.08)
val alpha = 0.05

// Sort p-values
val sortedWithIndex = pValues.zipWithIndex.sortBy(_._1)

// Find largest i where p_i <= (i/m) * alpha
val m = pValues.length
val threshold = sortedWithIndex.zipWithIndex.findLast { case ((p, _), i) =>
  p <= ((i + 1).toDouble / m) * alpha
}

threshold match {
  case Some(((maxP, _), _)) =>
    val significant = pValues.filter(_ <= maxP)
    println(s"Significant tests (BH): ${significant.length} out of $m")
  case None =>
    println("No significant tests after BH correction")
}
```

## Power Analysis

Estimate required sample size or power of a test:

```scala
import au.id.cxd.math.probability.continuous.{Normal, StudentT}

// Parameters
val alpha = 0.05
val power = 0.80
val effectSize = 0.5  // Cohen's d

// For t-test
val zAlpha = Normal(0, 1).invcdf(1 - alpha / 2)
val zBeta = Normal(0, 1).invcdf(power)

val requiredN = 2 * math.pow((zAlpha + zBeta) / effectSize, 2)

println(f"Required sample size per group: ${requiredN.ceil.toInt}")
```

## Best Practices

1. **Check assumptions** before running tests
2. **Use appropriate tests** for your data type
3. **Visualize data** before and after testing
4. **Correct for multiple comparisons** when doing many tests
5. **Report effect sizes** along with p-values
6. **Consider power analysis** when designing studies

## Common Pitfalls

- ❌ Using parametric tests on non-normal data
- ❌ Not checking for equal variances (homoscedasticity)
- ❌ Running many tests without correction
- ❌ Confusing statistical with practical significance
- ❌ P-hacking (trying tests until you get p < 0.05)

## See Also

- [Probability Distributions](Probability-Distributions.md) - Understanding test distributions
- [Multivariate Analysis](Multivariate-Analysis.md) - Multivariate methods
- [Examples Catalog](Examples-Catalog.md) - Working examples
- [API Quick Reference](API-Quick-Reference.md) - Quick lookup

---

[← Back to Home](Home.md)

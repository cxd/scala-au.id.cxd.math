# Getting Started

This guide will help you get started with the **scala-au.id.cxd.math** library.

## Prerequisites

- **JDK 8** - The library is currently dependent on JDK 8
- **SBT** - Scala Build Tool for compilation
- **Scala 2.11.12, 2.12.17, or 2.13.10** - Cross-compiled for multiple Scala versions

### Managing Java Versions

If you have multiple Java versions installed, use `jenv` to manage them:

```bash
# Check available versions
jenv versions

# Set Java 8 for this project
jenv local 1.8
```

## Installation

### Building from Source

1. **Clone the repository**:
```bash
git clone https://github.com/cxd/scala-au.id.cxd.math.git
cd scala-au.id.cxd.math
```

2. **Compile and publish locally**:
```bash
sbt compile package publishLocal
```

This command gets around dependency resolution issues during cross-compilation.

3. **Build cross-compiled assemblies** (optional):
```bash
sbt +assembly
```

This creates JAR files for all supported Scala versions.

### Using in Your Project

Add to your `build.sbt`:

```scala
libraryDependencies += "au.id.cxd" %% "math" % "1.0"
```

## Project Structure

```
scala-au.id.cxd.math/
├── math/              # Core math library
├── examples/          # Usage examples
├── app/               # Swing UI application
├── docs/              # Documentation and reference materials
└── wiki-docs/         # This wiki documentation
```

## Generating Documentation

Generate API documentation with MathJax support for formulas:

```bash
sbt mathFormulaInDoc
```

This creates HTML documentation with properly rendered mathematical formulas.

## First Steps

### 1. Working with Probability Distributions

```scala
import au.id.cxd.math.probability.continuous.Normal
import au.id.cxd.math.probability.discrete.Binomial

// Normal distribution
val normal = Normal(mu = 0.0, sigma = 1.0)
val pdfValue = normal.pdf(1.5)       // Probability density at x=1.5
val cdfValue = normal.cdf(1.5)       // Cumulative probability
val samples = normal.draw(100)       // Generate 100 random samples

// Binomial distribution
val binomial = Binomial(n = 10, p = 0.5)
val prob = binomial.pdf(5)           // P(X = 5)
val cumProb = binomial.cdf(5)        // P(X <= 5)
```

### 2. Linear Regression

```scala
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._

// Prepare data
val X = DenseMatrix((1.0, 2.0), (2.0, 3.0), (3.0, 4.0))
val y = DenseVector(2.0, 3.0, 4.0)

// Fit model
val regression = LinearRegression(y, X)
val coefficients = regression.beta
val predictions = regression.predict
val rSquared = regression.rSquared
```

### 3. Principal Component Analysis

```scala
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.PrincipalComponentsAnalysis
import breeze.linalg._

// Standardize data
val data = DenseMatrix.rand(100, 5)  // 100 samples, 5 features
val normalized = StandardisedNormalisation(data).transform(data)

// Run PCA
val pca = PrincipalComponentsAnalysis(normalized)
val eigenvalues = pca.eigenValues
val eigenvectors = pca.eigenVectors
val varExplained = pca.varianceExplained
```

### 4. Text Processing with LSI

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.count.DocumentTermVectoriser

// Prepare documents
val documents = Seq(
  "the quick brown fox",
  "jumped over the lazy dog",
  "the dog was not amused"
)

// Create LSI model
val vectoriser = DocumentTermVectoriser()
val (matrix, dictionary) = vectoriser.makeDocTermMatrix(documents)
val lsi = LatentSemanticIndex(matrix, k = 2)  // 2 components

val transformed = lsi.transform(matrix)
```

## Running Examples

The `examples` directory contains many working examples:

```bash
# Run all examples (as tests)
sbt examples/test

# Run specific example
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleCarsStopDist"
```

See the [Examples Catalog](Examples-Catalog.md) for a complete list.

## Next Steps

- **[Quick Start Examples](Quick-Start-Examples.md)** - More code examples
- **[Probability Distributions](Probability-Distributions.md)** - Learn about available distributions
- **[Regression Methods](Regression-Methods.md)** - Explore regression techniques
- **[Neural Networks](Neural-Networks.md)** - Build neural networks
- **[API Quick Reference](API-Quick-Reference.md)** - Common operations reference

## Getting Help

- Check the [API Documentation](https://cxd.github.io/scala-au.id.cxd.math/latest/math/api/index.html)
- Browse the [Examples Catalog](Examples-Catalog.md)
- Open an issue on [GitHub](https://github.com/cxd/scala-au.id.cxd.math/issues)

---

[← Back to Home](Home.md)

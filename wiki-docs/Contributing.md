# Contributing Guidelines

Thank you for your interest in contributing to **scala-au.id.cxd.math**!

## About This Project

This is an **experimental hobby project** maintained by a single developer. Contributions are welcome, but please understand:

- The project is not actively maintained on a regular schedule
- Changes may take time to review
- The project is for learning and experimentation, not production use
- Code quality and accuracy may vary

## Ways to Contribute

### 1. Report Issues

Found a bug or have a suggestion?

- **Check existing issues** first to avoid duplicates
- **Provide details**: What did you expect? What happened instead?
- **Include code**: Minimal reproducible example
- **Specify versions**: Scala version, library version, JDK version

**Template**:
```markdown
## Description
Brief description of the issue

## Expected Behavior
What should happen

## Actual Behavior
What actually happens

## Code to Reproduce
```scala
// Minimal example
```

## Environment
- Scala version: 2.11.12
- JDK version: 1.8
- OS: Ubuntu 20.04
```

### 2. Improve Documentation

Documentation contributions are highly valued!

- **Fix typos** and clarify confusing sections
- **Add examples** to existing documentation
- **Write tutorials** for complex features
- **Improve API docs** with better descriptions
- **Enhance wiki pages** with more details

### 3. Add Examples

Help others learn by contributing examples:

- Create new example programs
- Add real-world use cases
- Provide visualizations
- Write tutorials combining multiple features

See [Examples Catalog](Examples-Catalog.md) for existing examples.

### 4. Submit Code

Want to contribute code?

**Types of contributions**:
- Bug fixes
- New algorithms or methods
- Performance improvements
- Additional tests
- Code refactoring

**Before writing code**:
1. **Open an issue** to discuss your proposed change
2. **Wait for feedback** from the maintainer
3. **Fork the repository** and create a branch
4. **Write your code** following the guidelines below

## Development Setup

### Prerequisites

```bash
# JDK 8
java -version  # Should show 1.8.x

# SBT
sbt version

# Git
git --version
```

### Clone and Build

```bash
# Fork the repository on GitHub first

# Clone your fork
git clone https://github.com/YOUR_USERNAME/scala-au.id.cxd.math.git
cd scala-au.id.cxd.math

# Set JDK 8 (if using jenv)
jenv local 1.8

# Build
sbt compile package publishLocal

# Run tests
sbt math/test
sbt examples/test
```

## Code Guidelines

### Scala Style

Follow standard Scala conventions:

```scala
// Good: camelCase for methods and variables
def calculateMean(values: DenseVector[Double]): Double = ...

// Good: PascalCase for classes and traits
class NormalDistribution(mu: Double, sigma: Double) extends Distribution

// Good: Descriptive names
val standardDeviation = math.sqrt(variance)

// Bad: Single letter names (except in math formulas or very local scope)
val s = math.sqrt(v)  // Avoid unless in tight loop or math context
```

### Documentation

Add ScalaDoc comments to public APIs:

```scala
/**
 * Computes the probability density function for the normal distribution.
 *
 * The PDF is given by:
 * ##import MathJax
 * $$f(x) = \frac{1}{\sigma\sqrt{2\pi}} e^{-\frac{(x-\mu)^2}{2\sigma^2}}$$
 *
 * @param x The point at which to evaluate the PDF
 * @return The probability density at x
 * @example {{{
 * val normal = Normal(mu = 0.0, sigma = 1.0)
 * val density = normal.pdf(1.5)
 * }}}
 */
def pdf(x: Double): Double = ...
```

**Note**: Use `##import MathJax` to enable LaTeX rendering in docs.

### Testing

Write tests for new functionality:

```scala
package au.id.cxd.math.probability.continuous

import org.scalatest.{FlatSpec, Matchers}

class NormalTest extends FlatSpec with Matchers {
  
  "Normal distribution" should "have correct mean" in {
    val normal = Normal(mu = 5.0, sigma = 2.0)
    normal.mean() should be (5.0 +- 0.001)
  }
  
  it should "have correct variance" in {
    val normal = Normal(mu = 5.0, sigma = 2.0)
    normal.variance() should be (4.0 +- 0.001)
  }
  
  it should "calculate PDF correctly" in {
    val normal = Normal(mu = 0.0, sigma = 1.0)
    val pdf = normal.pdf(0.0)
    pdf should be (0.3989 +- 0.001)  // 1/sqrt(2π)
  }
}
```

### Performance

- **Avoid unnecessary copying** of large matrices
- **Use in-place operations** when possible with Breeze
- **Profile before optimizing** - don't guess
- **Document complexity** of algorithms in comments

```scala
// Good: In-place operation
val result = A :*= 2.0  // Modifies A

// Less efficient: Creates new matrix
val result = A * 2.0
```

## Pull Request Process

### 1. Create a Branch

```bash
git checkout -b feature/my-new-feature
# or
git checkout -b fix/bug-description
```

### 2. Make Changes

- Write clean, well-documented code
- Add tests for new functionality
- Update documentation as needed
- Keep commits focused and logical

### 3. Test Your Changes

```bash
# Run tests
sbt math/test

# Run specific test
sbt "math/testOnly au.id.cxd.math.probability.continuous.NormalTest"

# Run examples
sbt examples/test

# Generate docs
sbt mathFormulaInDoc
```

### 4. Commit

Write clear commit messages:

```bash
# Good
git commit -m "Add support for multivariate Student's t-distribution"
git commit -m "Fix numerical stability in Gamma function for large values"
git commit -m "Update documentation for LogisticRegression class"

# Bad
git commit -m "Fixed stuff"
git commit -m "WIP"
```

### 5. Push and Create PR

```bash
git push origin feature/my-new-feature
```

Then create a Pull Request on GitHub with:

**Title**: Brief description of change

**Description**:
```markdown
## What does this PR do?
Brief description

## Why is this change needed?
Motivation and context

## How has this been tested?
Description of tests

## Types of changes
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to change)
- [ ] Documentation update

## Checklist
- [ ] My code follows the code style of this project
- [ ] I have added tests to cover my changes
- [ ] All new and existing tests passed
- [ ] I have updated the documentation accordingly
```

### 6. Code Review

- The maintainer will review your PR
- Address any feedback or requested changes
- Be patient - this is a hobby project

## Project Structure

Understanding the codebase:

```
scala-au.id.cxd.math/
├── math/                  # Core library
│   └── src/
│       ├── main/scala/    # Source code
│       │   └── au/id/cxd/math/
│       │       ├── probability/    # Distributions
│       │       ├── function/       # Mathematical functions
│       │       ├── model/          # ML models
│       │       ├── data/           # Data I/O
│       │       └── count/          # Combinatorics
│       └── test/scala/    # Unit tests
├── examples/              # Example programs
│   └── src/test/scala/   # Examples as tests
├── app/                   # Swing UI application
├── docs/                  # Documentation
├── wiki-docs/             # Wiki documentation
└── build.sbt              # Build configuration
```

## Adding a New Distribution

Example of adding a new distribution:

```scala
package au.id.cxd.math.probability.continuous

import breeze.linalg._
import au.id.cxd.math.probability.Distribution

/**
 * Weibull distribution with shape parameter k and scale parameter λ.
 *
 * @param k Shape parameter (k > 0)
 * @param lambda Scale parameter (λ > 0)
 */
case class Weibull(k: Double, lambda: Double) extends ContinuousDistribution {
  
  require(k > 0, "Shape parameter k must be positive")
  require(lambda > 0, "Scale parameter lambda must be positive")
  
  /**
   * Probability density function.
   * ##import MathJax
   * $$f(x) = \frac{k}{\lambda}\left(\frac{x}{\lambda}\right)^{k-1}e^{-(x/\lambda)^k}$$
   */
  def pdf(x: Double): Double = {
    if (x < 0) 0.0
    else (k / lambda) * math.pow(x / lambda, k - 1) * math.exp(-math.pow(x / lambda, k))
  }
  
  /**
   * Cumulative distribution function.
   * ##import MathJax
   * $$F(x) = 1 - e^{-(x/\lambda)^k}$$
   */
  def cdf(x: Double): Double = {
    if (x < 0) 0.0
    else 1.0 - math.exp(-math.pow(x / lambda, k))
  }
  
  /**
   * Inverse CDF (quantile function).
   */
  def invcdf(p: Double): Double = {
    require(p >= 0 && p <= 1, "Probability must be in [0,1]")
    lambda * math.pow(-math.log(1 - p), 1.0 / k)
  }
  
  /**
   * Generate random sample.
   */
  def draw(): Double = invcdf(scala.util.Random.nextDouble())
  
  /**
   * Expected value.
   */
  def mean(): Double = {
    lambda * Gamma.gamma(1 + 1.0 / k)
  }
  
  /**
   * Variance.
   */
  def variance(): Double = {
    val mean = lambda * Gamma.gamma(1 + 1.0 / k)
    lambda * lambda * Gamma.gamma(1 + 2.0 / k) - mean * mean
  }
}
```

Add tests:

```scala
class WeibullTest extends FlatSpec with Matchers {
  "Weibull distribution" should "have correct mean" in {
    val weibull = Weibull(k = 2.0, lambda = 1.0)
    weibull.mean() should be (0.8862 +- 0.001)
  }
  
  it should "have PDF that integrates to 1" in {
    val weibull = Weibull(k = 2.0, lambda = 1.0)
    val samples = (0.0 to 10.0 by 0.01).map(x => weibull.pdf(x) * 0.01).sum
    samples should be (1.0 +- 0.01)
  }
}
```

## Getting Help

- **Open an issue** for questions
- **Check existing examples** for patterns
- **Review the API documentation**
- **Ask in the PR** if you're unsure about something

## License

By contributing, you agree that your contributions will be licensed under the same license as the project (see LICENSE file).

## Recognition

Contributors will be acknowledged in the project!

## Thank You!

Every contribution, no matter how small, is appreciated. Thank you for helping improve this project!

---

[← Back to Home](Home.md)

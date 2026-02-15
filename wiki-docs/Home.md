# scala-au.id.cxd.math Wiki

Welcome to the **scala-au.id.cxd.math** documentation wiki! This is a comprehensive Scala library for statistical computing, machine learning, and mathematical operations.

## 🎯 Overview

This library provides a wide range of mathematical and statistical tools implemented in Scala, including:

- **Probability Distributions** - Both continuous and discrete distributions with PDF, CDF, and random generation
- **Statistical Tests** - ANOVA, MANOVA, normality tests, and more
- **Regression Methods** - OLS, logistic regression, Bayesian regression
- **Neural Networks** - Feedforward networks with multiple activation functions
- **Text Processing** - Latent Semantic Indexing (LSI), TF-IDF, text preprocessing
- **Multivariate Analysis** - PCA, discriminant analysis, clustering
- **Advanced Functions** - Gamma, Beta, special functions, distance metrics

## 📚 Documentation

### Getting Started
- **[Getting Started Guide](Getting-Started.md)** - Installation, building, and first steps
- **[Quick Start Examples](Quick-Start-Examples.md)** - Jump right in with code examples

### Core Modules
- **[Probability Distributions](Probability-Distributions.md)** - Complete guide to distributions
- **[Statistical Tests](Statistical-Tests.md)** - Hypothesis testing and inference
- **[Regression Methods](Regression-Methods.md)** - Linear and logistic regression
- **[Neural Networks](Neural-Networks.md)** - Building and training neural networks
- **[Text Processing](Text-Processing.md)** - NLP and text analysis tools
- **[Multivariate Analysis](Multivariate-Analysis.md)** - PCA, discriminant analysis, clustering
- **[Data Processing](Data-Processing.md)** - Data loading, preprocessing, and transformation

### Reference
- **[API Quick Reference](API-Quick-Reference.md)** - Common operations and classes
- **[Examples Catalog](Examples-Catalog.md)** - Complete list of working examples
- **[Mathematical Functions](Mathematical-Functions.md)** - Special functions and utilities

### Development
- **[Contributing Guidelines](Contributing.md)** - How to contribute to the project
- **[Architecture Overview](Architecture.md)** - Design patterns and structure
- **[TODOs and Future Work](TODOs.md)** - Planned improvements

## 🔗 External Resources

- **[GitHub Repository](https://github.com/cxd/scala-au.id.cxd.math)**
- **[API Documentation](https://cxd.github.io/scala-au.id.cxd.math/latest/math/api/index.html)**
- **[GitHub Pages](https://cxd.github.io/scala-au.id.cxd.math/)**

## ⚠️ Important Notes

This library is an **experimental hobby project** maintained by a single developer. It is:

- ✅ Great for learning and experimentation
- ✅ Useful for prototyping statistical algorithms
- ✅ Well-documented with examples
- ⚠️ **Not recommended for production use**
- ⚠️ May have lower accuracy than mature libraries (R, SciPy, GSL)
- ⚠️ Not actively maintained on a regular schedule

## 🚀 Quick Example

```scala
import au.id.cxd.math.probability.continuous.Normal

// Create a normal distribution
val normal = Normal(mu = 0.0, sigma = 1.0)

// Calculate probability density
val pdf = normal.pdf(1.5)

// Calculate cumulative probability
val cdf = normal.cdf(1.5)

// Generate random samples
val samples = normal.draw(1000)
```

## 📧 Contact

If you're using this library or have feedback, please open an issue on GitHub!

---

**Note**: This wiki can be browsed directly in the repository under `/wiki-docs/` or copied to the GitHub wiki for easier navigation.

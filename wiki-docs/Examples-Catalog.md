# Examples Catalog

A comprehensive catalog of all working examples included in this library.

## Repository Location

All examples are located in: `/examples/src/test/scala/au/id/cxd/math/example/`

## Running Examples

Examples are implemented as ScalaTest test cases:

```bash
# Run all examples
sbt examples/test

# Run specific example
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleCarsStopDist"

# Run all examples in a package
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.*"
```

## Regression Examples

### Linear Regression

#### `ExampleCarsStopDist`
**File**: `probability/regression/ExampleCarsStopDist.scala`

Demonstrates simple linear regression using the `cars` dataset (speed vs stopping distance).

**Key concepts**:
- Simple linear regression
- Scatter plots with regression line
- Residual analysis
- R-squared calculation

```bash
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleCarsStopDist"
```

#### `ExampleMtCarsConsumption`
**File**: `probability/regression/ExampleMtCarsConsumption.scala`

Multiple linear regression on the mtcars dataset.

**Key concepts**:
- Multiple predictors
- Model diagnostics
- Coefficient interpretation
- Adjusted R-squared

```bash
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleMtCarsConsumption"
```

#### `ExampleSinWave`
**File**: `probability/regression/ExampleSinWave.scala`

Regression on synthetic sinusoidal data to demonstrate overfitting and polynomial regression.

**Key concepts**:
- Polynomial features
- Overfitting vs underfitting
- Non-linear relationships

```bash
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleSinWave"
```

### Logistic Regression

#### `ExampleCarsLogitRegression`
**File**: `probability/regression/ExampleCarsLogitRegression.scala`

Binary classification using logistic regression.

**Key concepts**:
- Binary classification
- Probability predictions
- Decision boundaries
- Classification accuracy

```bash
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleCarsLogitRegression"
```

### Bayesian Regression

#### `ExampleMtCarsBayesRegression`
**File**: `probability/regression/ExampleMtCarsBayesRegression.scala`

Bayesian linear regression with uncertainty quantification.

**Key concepts**:
- Bayesian inference
- Posterior distributions
- Uncertainty estimates
- Prior specification

```bash
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleMtCarsBayesRegression"
```

#### `ExampleCarsLogitBayesRegression`
**File**: `probability/regression/ExampleCarsLogitBayesRegression.scala`

Bayesian logistic regression for classification.

**Key concepts**:
- Bayesian classification
- Posterior predictive distributions
- Model uncertainty

```bash
sbt "examples/testOnly au.id.cxd.math.example.probability.regression.ExampleCarsLogitBayesRegression"
```

## Neural Network Examples

### `ExampleCarsRegressionNetwork`
**File**: `network/ExampleCarsRegressionNetwork.scala`

Neural network for regression tasks.

**Key concepts**:
- Network architecture design
- Activation functions
- Training with SGD
- Loss functions
- Visualization of training progress

```bash
sbt "examples/testOnly au.id.cxd.math.example.network.ExampleCarsRegressionNetwork"
```

### `ExampleMulticlassNetwork`
**File**: `network/ExampleMulticlassNetwork.scala`

Multi-class classification with neural networks.

**Key concepts**:
- One-hot encoding
- Softmax activation
- Multi-class loss functions
- Confusion matrices

```bash
sbt "examples/testOnly au.id.cxd.math.example.network.ExampleMulticlassNetwork"
```

### `ExampleMultinomialLogisticRegression`
**File**: `network/ExampleMultinomialLogisticRegression.scala`

Multinomial logistic regression (softmax regression).

**Key concepts**:
- Multi-class logistic regression
- Softmax probabilities
- Maximum likelihood estimation

```bash
sbt "examples/testOnly au.id.cxd.math.example.network.ExampleMultinomialLogisticRegression"
```

## Text Processing Examples

### Latent Semantic Indexing (LSI)

#### `LsiModelExample`
**File**: `text/model/LsiModelExample.scala`

Build an LSI model for document analysis.

**Key concepts**:
- Document-term matrices
- TF-IDF weighting
- Singular Value Decomposition
- Dimensionality reduction
- Document similarity

```bash
sbt "examples/testOnly au.id.cxd.math.example.text.model.LsiModelExample"
```

#### `LsiModelWriteExample`
**File**: `text/model/LsiModelWriteExample.scala`

Save an LSI model to disk.

**Key concepts**:
- Model serialization
- Persistence
- Model versioning

```bash
sbt "examples/testOnly au.id.cxd.math.example.text.model.LsiModelWriteExample"
```

#### `LsiReadModelExample`
**File**: `text/model/LsiReadModelExample.scala`

Load a saved LSI model from disk.

**Key concepts**:
- Model deserialization
- Loading persisted models
- Model reuse

```bash
sbt "examples/testOnly au.id.cxd.math.example.text.model.LsiReadModelExample"
```

#### `LsiKMeansExample`
**File**: `text/model/LsiKMeansExample.scala`

Combine LSI with K-Means clustering.

**Key concepts**:
- Document clustering
- LSI for preprocessing
- K-Means algorithm
- Cluster interpretation

```bash
sbt "examples/testOnly au.id.cxd.math.example.text.model.LsiKMeansExample"
```

#### `LsiModelClusterExample`
**File**: `text/model/LsiModelClusterExample.scala`

Advanced document clustering with LSI.

**Key concepts**:
- Hierarchical clustering
- Cluster analysis
- Document grouping

```bash
sbt "examples/testOnly au.id.cxd.math.example.text.model.LsiModelClusterExample"
```

## MCMC Examples

### `ExampleNR`
**File**: `model/mcmc/nr/ExampleNR.scala`

Markov Chain Monte Carlo sampling examples.

**Key concepts**:
- Metropolis-Hastings algorithm
- Gibbs sampling
- MCMC diagnostics
- Posterior sampling

```bash
sbt "examples/testOnly au.id.cxd.math.example.model.mcmc.nr.ExampleNR"
```

## Charting and Visualization

### `ChartHelper`
**File**: `charting/ChartHelper.scala`

Utilities for creating charts with JFreeChart and Breeze-viz.

**Key concepts**:
- Scatter plots
- Line plots
- Histograms
- Custom styling

### `VegasHelper`
**File**: `charting/VegasHelper.scala`

Utilities for creating interactive visualizations with Vegas.

**Key concepts**:
- Interactive charts
- Vegas DSL
- JSON specifications
- Web-based visualization

## Example Data

Common datasets used in examples:

### Cars Dataset
- **Variables**: speed, stopping distance
- **Size**: 50 observations
- **Use**: Simple linear regression
- **Location**: Embedded in examples

### mtcars Dataset
- **Variables**: mpg, cylinders, displacement, horsepower, weight, etc.
- **Size**: 32 observations
- **Use**: Multiple regression, multivariate analysis
- **Location**: Embedded in examples

### Iris Dataset
- **Variables**: sepal length, sepal width, petal length, petal width, species
- **Size**: 150 observations
- **Use**: Classification, PCA, discriminant analysis
- **Location**: Often generated or embedded

## Creating Your Own Examples

To create a new example:

1. **Create a test file** in `examples/src/test/scala/`:
```scala
package au.id.cxd.math.example.myexample

import org.scalatest.{FlatSpec, Matchers}
import au.id.cxd.math.probability.continuous.Normal

class MyExample extends FlatSpec with Matchers {
  
  "My Example" should "demonstrate something useful" in {
    val normal = Normal(0.0, 1.0)
    val prob = normal.cdf(1.96)
    
    println(s"P(X <= 1.96) = $prob")
    
    // Assertions
    prob should be > 0.97
    prob should be < 0.98
  }
}
```

2. **Run your example**:
```bash
sbt "examples/testOnly au.id.cxd.math.example.myexample.MyExample"
```

## Example Output

Most examples produce:
- **Console output**: Statistics, summaries, results
- **HTML files**: Interactive plots (saved to project root)
- **CSV files**: Exported data and results

### Viewing HTML Plots

After running examples that generate plots:

```bash
# HTML files are saved to project root
ls -l *.html

# Open in browser
open plot*.html  # macOS
xdg-open plot*.html  # Linux
```

## Tips for Working with Examples

1. **Start simple**: Begin with `ExampleCarsStopDist`
2. **Read the code**: Examples are well-commented
3. **Modify parameters**: Change values to see effects
4. **Compare outputs**: Run multiple versions
5. **Use as templates**: Copy and adapt for your needs

## Configuration

Some examples use configuration files:

**File**: `examples/src/test/resources/application.conf`

```hocon
example {
  data.path = "data/"
  output.path = "output/"
  chart.width = 800
  chart.height = 600
}
```

## Troubleshooting

### Example won't run

- Check JDK version (must be JDK 8)
- Run `sbt clean compile` first
- Check data file paths

### Chart not displaying

- Check HTML file location (project root)
- Verify JavaScript is enabled in browser
- Try a different browser

### Out of memory errors

Increase heap size in `build.sbt`:
```scala
javaOptions += "-Xmx4G"
```

## Contributing Examples

Want to add your own example?

1. Create your example following the patterns above
2. Test thoroughly
3. Add documentation
4. Submit a pull request

See [Contributing Guidelines](Contributing.md) for details.

## See Also

- [Quick Start Examples](Quick-Start-Examples.md) - Code snippets
- [API Quick Reference](API-Quick-Reference.md) - API lookup
- [Probability Distributions](Probability-Distributions.md) - Distribution examples
- [Regression Methods](Regression-Methods.md) - Regression examples

---

[← Back to Home](Home.md)

# Regression Methods

This library provides various regression techniques for modeling relationships between variables.

## Overview

Regression methods in this library include:
- **Ordinary Least Squares (OLS)** - Classical linear regression
- **Logistic Regression** - Binary and multinomial classification
- **Bayesian Regression** - Regression with uncertainty quantification
- **Neural Network Regression** - Non-linear regression with deep learning

## Linear Regression

### Ordinary Least Squares (OLS)

The most common regression method for modeling linear relationships.

**Package**: `au.id.cxd.math.function.approximate.LinearRegression`

#### Simple Linear Regression

One predictor variable:

```scala
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._

// Data: x and y
val x = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)
val y = DenseVector(2.0, 4.0, 5.0, 4.0, 5.0)

// Create design matrix (add intercept)
val X = DenseMatrix.horzcat(
  DenseMatrix.ones[Double](x.length, 1),
  x.asDenseMatrix.t
)

// Fit model
val model = LinearRegression(y, X)

// Results
println(s"Intercept: ${model.beta(0)}")
println(s"Slope: ${model.beta(1)}")
println(s"R-squared: ${model.rSquared}")
println(s"Adjusted R-squared: ${model.adjustedRSquared}")
println(s"Residual standard error: ${model.residualStandardError}")

// Make predictions
val newX = DenseMatrix((1.0, 6.0))
val prediction = model.op(newX)
println(s"Predicted y for x=6: ${prediction(0)}")
```

#### Multiple Linear Regression

Multiple predictor variables:

```scala
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._

// Multiple predictors
val y = DenseVector(10.0, 12.0, 15.0, 18.0, 20.0)
val X = DenseMatrix(
  (1.0, 2.0, 3.0),  // Intercept, x1, x2
  (1.0, 3.0, 4.0),
  (1.0, 4.0, 5.0),
  (1.0, 5.0, 6.0),
  (1.0, 6.0, 7.0)
)

val model = LinearRegression(y, X)

println(s"Coefficients: ${model.beta}")
println(s"R-squared: ${model.rSquared}")

// Partial F-test for model significance
println(s"F-statistic: ${model.fStatistic}")
println(s"p-value: ${model.pValue}")
```

#### Model Diagnostics

```scala
val model = LinearRegression(y, X)

// Residuals
val residuals = model.residuals
val fitted = model.predict

// Residual plots (using breeze-viz or your plotting library)
// Plot residuals vs fitted values
// Plot Q-Q plot of residuals
// Plot residuals vs leverage

// Influence measures
val leverage = model.leverage  // Hat values
val standardizedResiduals = model.standardizedResiduals
val studentizedResiduals = model.studentizedResiduals

// Cook's distance (measure of influence)
val cooksDistance = model.cooksDistance
```

#### Polynomial Regression

```scala
import breeze.linalg._

// Transform data to polynomial features
def polynomialFeatures(x: DenseVector[Double], degree: Int): DenseMatrix[Double] = {
  val features = (0 to degree).map { d =>
    x.map(v => math.pow(v, d))
  }
  DenseMatrix.horzcat(features.map(_.asDenseMatrix.t): _*)
}

val x = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)
val y = DenseVector(2.0, 8.0, 18.0, 32.0, 50.0)  // Quadratic relationship

// Create polynomial features (degree 2)
val X = polynomialFeatures(x, degree = 2)

val model = LinearRegression(y, X)
println(s"Polynomial coefficients: ${model.beta}")
println(s"R-squared: ${model.rSquared}")
```

### Weighted Least Squares

For heteroscedastic data:

```scala
import breeze.linalg._

// Weights (inverse of variance)
val weights = DenseVector(1.0, 1.0, 0.5, 0.5, 0.5)

// Transform data by sqrt(weights)
val W = diag(weights.map(math.sqrt))
val y_weighted = W * y
val X_weighted = W * X

val model = LinearRegression(y_weighted, X_weighted)
```

## Logistic Regression

For binary classification problems.

**Package**: `au.id.cxd.math.function.approximate.LogisticRegression`

### Binary Logistic Regression

```scala
import au.id.cxd.math.function.approximate.LogisticRegression
import breeze.linalg._

// Features
val X = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 3.0),
  (4.0, 5.0),
  (5.0, 6.0),
  (6.0, 7.0)
)

// Binary labels (0 or 1)
val y = DenseVector(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)

// Fit model
val logit = LogisticRegression(y, X)

// Get coefficients
val coefficients = logit.beta
println(s"Coefficients: $coefficients")

// Predict probabilities
val probabilities = logit.predict(X)
println(s"Predicted probabilities: $probabilities")

// Classify (threshold = 0.5)
val predictions = probabilities.map(p => if (p > 0.5) 1.0 else 0.0)
println(s"Classifications: $predictions")

// Evaluate accuracy
val accuracy = (predictions :== y).activeSize.toDouble / y.length
println(f"Accuracy: ${accuracy * 100}%.1f%%")
```

### Odds Ratios

```scala
val logit = LogisticRegression(y, X)

// Odds ratios (exp of coefficients)
val oddsRatios = logit.beta.map(math.exp)
println(s"Odds ratios: $oddsRatios")

// Interpretation:
// oddsRatio(i) > 1: Feature i increases odds of positive class
// oddsRatio(i) < 1: Feature i decreases odds of positive class
// oddsRatio(i) = 1: Feature i has no effect
```

### Multinomial Logistic Regression

For multi-class classification:

**Package**: `au.id.cxd.math.function.approximate.MultinomialLogistic`

```scala
import au.id.cxd.math.function.approximate.MultinomialLogistic
import breeze.linalg._

// Features
val X = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 3.0),
  (4.0, 5.0),
  (5.0, 6.0),
  (6.0, 7.0)
)

// Multi-class labels (0, 1, 2)
val y = DenseVector(0.0, 0.0, 1.0, 1.0, 2.0, 2.0)

// Fit model
val model = MultinomialLogistic(y, X)

// Predict class probabilities
val probabilities = model.predict(X)
// Returns matrix where each row contains probabilities for each class

// Get predicted classes
val predictions = probabilities(*, ::).map(row => argmax(row))
```

## Bayesian Regression

Regression with uncertainty quantification using Bayesian inference.

### Bayesian Linear Regression

**Package**: `au.id.cxd.math.function.approximate.BayesianLinearRegression`

```scala
import au.id.cxd.math.function.approximate.BayesianLinearRegression
import breeze.linalg._

val X = DenseMatrix(
  (1.0, 1.0),
  (1.0, 2.0),
  (1.0, 3.0),
  (1.0, 4.0)
)
val y = DenseVector(2.0, 4.0, 6.0, 8.0)

// Prior hyperparameters
val alpha = 1.0  // Precision of prior on weights
val beta = 1.0   // Precision of noise

// Fit Bayesian model
val model = BayesianLinearRegression(y, X, alpha, beta)

// Posterior distribution over weights
val posteriorMean = model.mean
val posteriorCovariance = model.covariance

println(s"Posterior mean: $posteriorMean")
println(s"Posterior covariance:\n$posteriorCovariance")

// Predictive distribution
val newX = DenseMatrix((1.0, 5.0))
val (predMean, predVariance) = model.predictWithUncertainty(newX)

println(f"Prediction: $predMean%.2f")
println(f"Standard deviation: ${math.sqrt(predVariance)}%.2f")
println(f"95% CI: [${predMean - 1.96 * math.sqrt(predVariance)}%.2f, " +
        f"${predMean + 1.96 * math.sqrt(predVariance)}%.2f]")
```

### Bayesian Logistic Regression

**Package**: `au.id.cxd.math.function.approximate.BayesianLogisticRegression`

```scala
import au.id.cxd.math.function.approximate.BayesianLogisticRegression
import breeze.linalg._

val X = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 3.0),
  (4.0, 5.0)
)
val y = DenseVector(0.0, 0.0, 1.0, 1.0)

// Prior hyperparameter
val alpha = 1.0  // Precision of prior

// Fit Bayesian logistic regression
val model = BayesianLogisticRegression(y, X, alpha)

// Get posterior parameters
val posteriorMean = model.mean
val posteriorCovariance = model.covariance

// Predict with uncertainty
val (predProb, predVar) = model.predictWithUncertainty(X)
```

## Ridge Regression (L2 Regularization)

Regularized regression to prevent overfitting.

```scala
import au.id.cxd.math.function.approximate.RidgeRegression
import breeze.linalg._

val X = DenseMatrix.rand[Double](100, 10)  // High-dimensional data
val y = DenseVector.rand[Double](100)

// Regularization parameter (lambda)
val lambda = 1.0

val model = RidgeRegression(y, X, lambda)

println(s"Coefficients: ${model.beta}")
println(s"R-squared: ${model.rSquared}")

// Cross-validation to select lambda
val lambdas = Seq(0.001, 0.01, 0.1, 1.0, 10.0, 100.0)
val errors = lambdas.map { lambda =>
  val model = RidgeRegression(y, X, lambda)
  val predictions = model.predict
  val mse = sum((predictions - y).map(x => x * x)) / y.length
  (lambda, mse)
}

val bestLambda = errors.minBy(_._2)._1
println(s"Best lambda: $bestLambda")
```

## Robust Regression

Regression methods resistant to outliers.

### Least Absolute Deviations (LAD)

Minimize sum of absolute residuals instead of squared residuals.

```scala
import breeze.linalg._
import breeze.optimize._

// Optimization problem: minimize |y - X*beta|
val objective = new DiffFunction[DenseVector[Double]] {
  def calculate(beta: DenseVector[Double]) = {
    val residuals = y - (X * beta)
    val loss = sum(residuals.map(math.abs))
    val grad = -X.t * residuals.map(r => if (r > 0) 1.0 else -1.0)
    (loss, grad)
  }
}

val initialBeta = DenseVector.zeros[Double](X.cols)
val result = minimize(objective, initialBeta)
val robustBeta = result

println(s"Robust coefficients: $robustBeta")
```

## Model Selection and Validation

### Cross-Validation

```scala
import breeze.linalg._

def kFoldCV(X: DenseMatrix[Double], y: DenseVector[Double], k: Int): Double = {
  val n = X.rows
  val foldSize = n / k
  val indices = (0 until n).toArray
  scala.util.Random.shuffle(indices.toSeq)
  
  val errors = (0 until k).map { fold =>
    val testIndices = indices.slice(fold * foldSize, (fold + 1) * foldSize)
    val trainIndices = indices.filterNot(testIndices.contains)
    
    val X_train = X(trainIndices, ::).toDenseMatrix
    val y_train = y(trainIndices).toDenseVector
    val X_test = X(testIndices, ::).toDenseMatrix
    val y_test = y(testIndices).toDenseVector
    
    val model = LinearRegression(y_train, X_train)
    val predictions = model.op(X_test)
    val mse = sum((predictions - y_test).map(x => x * x)) / y_test.length
    mse
  }
  
  errors.sum / k
}

val cvError = kFoldCV(X, y, k = 5)
println(f"5-fold CV MSE: $cvError%.4f")
```

### Information Criteria

```scala
val model = LinearRegression(y, X)

// AIC (Akaike Information Criterion)
val n = y.length
val k = model.beta.length
val rss = sum(model.residuals.map(x => x * x))
val aic = n * math.log(rss / n) + 2 * k

// BIC (Bayesian Information Criterion)
val bic = n * math.log(rss / n) + k * math.log(n)

println(f"AIC: $aic%.2f")
println(f"BIC: $bic%.2f")

// Lower values indicate better models
```

## Advanced Topics

### Generalized Linear Models (GLM)

Framework for models with non-normal error distributions.

**Components**:
1. **Random component**: Distribution of Y (e.g., Binomial, Poisson)
2. **Systematic component**: Linear predictor η = Xβ
3. **Link function**: Relates E(Y) to linear predictor

**Common GLMs**:
- Linear regression: Normal distribution, identity link
- Logistic regression: Binomial distribution, logit link
- Poisson regression: Poisson distribution, log link

### Nonparametric Regression

For complex, non-linear relationships:

- **LOESS/LOWESS**: Locally weighted regression
- **Kernel regression**: Weighted average based on kernel function
- **Splines**: Piecewise polynomials

(See Neural Networks for flexible non-linear regression)

## Best Practices

1. **Explore your data** first (scatter plots, distributions)
2. **Check assumptions** (linearity, normality, homoscedasticity)
3. **Handle missing data** appropriately
4. **Standardize features** when using regularization
5. **Use cross-validation** to assess generalization
6. **Interpret coefficients** in context
7. **Report uncertainty** (confidence/prediction intervals)
8. **Validate on held-out test set**

## Common Pitfalls

- ❌ Extrapolating beyond data range
- ❌ Ignoring multicollinearity
- ❌ Using too many features (overfitting)
- ❌ Not checking residual plots
- ❌ Treating correlation as causation
- ❌ Ignoring influential observations
- ❌ Not scaling features in regularized models

## See Also

- [Probability Distributions](Probability-Distributions.md) - Distributions in regression
- [Statistical Tests](Statistical-Tests.md) - Testing regression assumptions
- [Neural Networks](Neural-Networks.md) - Non-linear regression
- [Examples Catalog](Examples-Catalog.md) - Working regression examples
- [Data Processing](Data-Processing.md) - Preparing data for regression

---

[← Back to Home](Home.md)

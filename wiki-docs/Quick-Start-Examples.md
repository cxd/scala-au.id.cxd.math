# Quick Start Examples

Jump right into using the library with these working examples!

## Table of Contents

1. [Probability Distributions](#probability-distributions)
2. [Statistical Tests](#statistical-tests)
3. [Linear Regression](#linear-regression)
4. [Logistic Regression](#logistic-regression)
5. [Neural Networks](#neural-networks)
6. [Principal Component Analysis](#principal-component-analysis)
7. [Text Processing with LSI](#text-processing-with-lsi)
8. [Clustering](#clustering)

## Probability Distributions

### Computing Probabilities

```scala
import au.id.cxd.math.probability.continuous.Normal
import au.id.cxd.math.probability.discrete.Binomial

// Normal distribution
val normal = Normal(mu = 100, sigma = 15)

// What's the probability of scoring between 85 and 115?
val lowerProb = normal.cdf(85)
val upperProb = normal.cdf(115)
val probability = upperProb - lowerProb
println(f"Probability: ${probability * 100}%.2f%%")

// Binomial: coin flips
val coin = Binomial(n = 10, p = 0.5)
val probExactly5Heads = coin.pdf(5)
val probAtMost3Heads = coin.cdf(3)
```

### Generating Random Data

```scala
import au.id.cxd.math.probability.continuous.{Normal, Exponential}
import breeze.linalg._

// Generate synthetic data
val normal = Normal(0.0, 1.0)
val data = DenseVector(normal.draw(1000))

// Exponential for event times
val exponential = Exponential(lambda = 0.5)
val eventTimes = DenseVector(exponential.draw(100))
```

## Statistical Tests

### One-Sample t-Test

```scala
import au.id.cxd.math.probability.continuous.StudentT
import breeze.stats._

val data = DenseVector(23.5, 24.1, 23.8, 24.3, 23.9, 24.2, 23.7)
val mu0 = 24.0  // Null hypothesis mean

val sampleMean = mean(data)
val sampleStd = stddev(data)
val n = data.length

val t = (sampleMean - mu0) / (sampleStd / math.sqrt(n))
val tDist = StudentT(n - 1)
val pValue = 2 * (1 - tDist.cdf(math.abs(t)))

println(f"t-statistic: $t%.3f")
println(f"p-value: $pValue%.4f")
```

### Chi-Square Test

```scala
import au.id.cxd.math.probability.continuous.ChiSquare

// Observed and expected frequencies
val observed = DenseVector(30.0, 35.0, 25.0, 40.0)
val expected = DenseVector(32.5, 32.5, 32.5, 32.5)

val chiSq = sum((observed - expected).map(x => x * x) / expected)
val df = observed.length - 1
val chiDist = ChiSquare(df)
val pValue = 1 - chiDist.cdf(chiSq)

println(f"Chi-square: $chiSq%.3f")
println(f"p-value: $pValue%.4f")
```

## Linear Regression

### Simple Linear Regression

```scala
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._

// Sample data: height (cm) vs weight (kg)
val heights = DenseVector(150.0, 160.0, 170.0, 180.0, 190.0)
val weights = DenseVector(50.0, 60.0, 70.0, 80.0, 90.0)

// Add intercept column
val X = DenseMatrix.horzcat(
  DenseMatrix.ones[Double](heights.length, 1),
  heights.asDenseMatrix.t
)

// Fit the model
val model = LinearRegression(weights, X)

println(s"Intercept: ${model.beta(0)}")
println(s"Slope: ${model.beta(1)}")
println(s"R-squared: ${model.rSquared}")

// Make predictions
val newHeight = DenseMatrix((1.0, 175.0))
val prediction = model.op(newHeight)
println(s"Predicted weight for 175cm: ${prediction(0)}kg")
```

### Multiple Linear Regression

```scala
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._

// Multiple predictors
val y = DenseVector(20.0, 25.0, 30.0, 35.0, 40.0)
val X = DenseMatrix(
  (1.0, 10.0, 5.0),  // Intercept, feature1, feature2
  (1.0, 15.0, 6.0),
  (1.0, 20.0, 7.0),
  (1.0, 25.0, 8.0),
  (1.0, 30.0, 9.0)
)

val model = LinearRegression(y, X)
println(s"Coefficients: ${model.beta}")
println(s"R-squared: ${model.rSquared}")
println(s"Adjusted R-squared: ${model.adjustedRSquared}")
```

## Logistic Regression

### Binary Classification

```scala
import au.id.cxd.math.function.approximate.LogisticRegression
import breeze.linalg._

// Features and binary labels
val X = DenseMatrix(
  (1.0, 2.0),
  (2.0, 3.0),
  (3.0, 3.0),
  (4.0, 5.0),
  (5.0, 6.0)
)
val y = DenseVector(0.0, 0.0, 1.0, 1.0, 1.0)

// Fit logistic regression
val logit = LogisticRegression(y, X)
val coefficients = logit.beta

// Predict probabilities
val predictions = logit.predict(X)
println(s"Predicted probabilities: $predictions")

// Classify (threshold = 0.5)
val classes = predictions.map(p => if (p > 0.5) 1.0 else 0.0)
```

## Neural Networks

### Simple Feedforward Network

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._
import au.id.cxd.math.model.network.loss.SquareError
import breeze.linalg._

// Build a network: 2 inputs -> 4 hidden -> 1 output
val network = Builder()
  .addLayer(Linear(inputSize = 2, outputSize = 4))
  .addLayer(Activation(Sigmoid))
  .addLayer(Linear(inputSize = 4, outputSize = 1))
  .addLayer(Activation(Sigmoid))
  .build()

// Training data
val X = DenseMatrix(
  (0.0, 0.0),
  (0.0, 1.0),
  (1.0, 0.0),
  (1.0, 1.0)
)
val y = DenseMatrix(
  (0.0),
  (1.0),
  (1.0),
  (0.0)
) // XOR problem

// Train the network
val trainer = SGDTrainer(
  learningRate = 0.5,
  epochs = 1000,
  lossFunction = SquareError
)

val trainedNetwork = trainer.train(network, X, y)

// Make predictions
val predictions = trainedNetwork.forward(X)
println(s"Predictions:\n$predictions")
```

## Principal Component Analysis

### Dimensionality Reduction

```scala
import au.id.cxd.math.model.components.PrincipalComponentsAnalysis
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg._

// Generate sample data (100 samples, 5 features)
val data = DenseMatrix.rand[Double](100, 5)

// Standardize the data
val normalizer = StandardisedNormalisation(data)
val standardized = normalizer.transform(data)

// Perform PCA
val pca = PrincipalComponentsAnalysis(standardized)

// Examine results
println(s"Eigenvalues: ${pca.eigenValues}")
println(s"Variance explained: ${pca.varianceExplained}")

// How many components for 95% variance?
val cumVar = pca.varianceExplained.scanLeft(0.0)(_ + _).tail
val numComponents = cumVar.indexWhere(_ >= 0.95) + 1
println(s"Components needed for 95% variance: $numComponents")

// Transform to reduced dimensions
val reduced = standardized * pca.eigenVectors(::, 0 until numComponents)
println(s"Original shape: ${standardized.rows} x ${standardized.cols}")
println(s"Reduced shape: ${reduced.rows} x ${reduced.cols}")
```

## Text Processing with LSI

### Document Similarity

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.count.DocumentTermVectoriser

// Sample documents
val documents = Seq(
  "The cat sat on the mat",
  "The dog played in the park",
  "Cats and dogs are pets",
  "Machine learning is awesome",
  "Deep learning uses neural networks"
)

// Create document-term matrix
val vectoriser = DocumentTermVectoriser()
val (docTermMatrix, dictionary) = vectoriser.makeDocTermMatrix(documents)

// Build LSI model with 2 latent dimensions
val lsi = LatentSemanticIndex(docTermMatrix, k = 2)

// Transform documents to latent space
val transformedDocs = lsi.transform(docTermMatrix)

println("Documents in latent space:")
println(transformedDocs)

// Calculate document similarities using cosine similarity
import au.id.cxd.math.function.distance.Cosine

val doc1 = transformedDocs(0, ::).t
val doc2 = transformedDocs(1, ::).t
val similarity = 1.0 - Cosine().measure(doc1, doc2)
println(f"Similarity between doc 1 and doc 2: $similarity%.3f")
```

## Clustering

### K-Means Clustering

```scala
import au.id.cxd.math.model.cluster.KMeans
import breeze.linalg._

// Generate sample data
val data = DenseMatrix(
  (1.0, 2.0),
  (1.5, 1.8),
  (5.0, 8.0),
  (8.0, 8.0),
  (1.0, 0.6),
  (9.0, 11.0)
)

// Run K-Means with 2 clusters
val kmeans = KMeans(k = 2, data)

// Get cluster assignments
val assignments = kmeans.assignments
println(s"Cluster assignments: $assignments")

// Get cluster centers
val centers = kmeans.centers
println(s"Cluster centers:\n$centers")

// Predict cluster for new point
val newPoint = DenseVector(2.0, 3.0)
val cluster = kmeans.predict(newPoint)
println(s"New point assigned to cluster: $cluster")
```

## Working with Data Files

### Loading CSV Data

```scala
import au.id.cxd.math.data.CsvReader
import java.io.File

// Read CSV file
val file = new File("data/dataset.csv")
val reader = new CsvReader()
val data = reader.read(file)

// Access data
val matrix = data.matrix        // DenseMatrix of numeric values
val headers = data.headers      // Column names
val rowLabels = data.rowLabels  // Row labels if present

println(s"Loaded ${matrix.rows} rows and ${matrix.cols} columns")
```

### Data Preprocessing

```scala
import au.id.cxd.math.function.transform._
import breeze.linalg._

val data = DenseMatrix.rand[Double](100, 5)

// Standardization (z-score)
val standardizer = StandardisedNormalisation(data)
val standardized = standardizer.transform(data)

// Min-max scaling to [0, 1]
val scaler = MinMaxNormalisation(data)
val scaled = scaler.transform(data)

// Logarithmic transformation
val logTransform = LogTransform()
val transformed = logTransform.transform(data)
```

## Bayesian Methods

### Bayesian Linear Regression

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

// Prior parameters
val alpha = 1.0  // Precision of prior on weights
val beta = 1.0   // Precision of noise

val bayesReg = BayesianLinearRegression(y, X, alpha, beta)

// Posterior mean and covariance
val posteriorMean = bayesReg.mean
val posteriorCov = bayesReg.covariance

println(s"Posterior mean: $posteriorMean")
println(s"Posterior covariance:\n$posteriorCov")

// Make predictions with uncertainty
val newX = DenseMatrix((1.0, 5.0))
val (predMean, predVar) = bayesReg.predictWithUncertainty(newX)
println(f"Prediction: $predMean%.2f ± ${math.sqrt(predVar)}%.2f")
```

## Next Steps

- **[Probability Distributions](Probability-Distributions.md)** - Complete distribution reference
- **[Statistical Tests](Statistical-Tests.md)** - Detailed testing procedures
- **[Regression Methods](Regression-Methods.md)** - Advanced regression techniques
- **[Neural Networks](Neural-Networks.md)** - Deep learning guide
- **[Examples Catalog](Examples-Catalog.md)** - Browse all available examples

---

[← Back to Home](Home.md)

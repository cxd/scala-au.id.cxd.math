# API Quick Reference

Quick reference guide for common operations and classes in the library.

## Table of Contents

1. [Probability Distributions](#probability-distributions)
2. [Statistical Functions](#statistical-functions)
3. [Linear Algebra](#linear-algebra)
4. [Regression](#regression)
5. [Neural Networks](#neural-networks)
6. [Text Processing](#text-processing)
7. [Data Processing](#data-processing)
8. [Distance Metrics](#distance-metrics)

## Probability Distributions

### Creating Distributions

```scala
// Continuous
import au.id.cxd.math.probability.continuous._

Normal(mu = 0.0, sigma = 1.0)
Uniform(a = 0.0, b = 1.0)
Exponential(lambda = 1.0)
Gamma(shape = 2.0, scale = 1.0)
Beta(alpha = 2.0, beta = 5.0)
ChiSquare(degreesOfFreedom = 10)
StudentT(degreesOfFreedom = 10)
FDistribution(df1 = 5, df2 = 10)

// Discrete
import au.id.cxd.math.probability.discrete._

Binomial(n = 10, p = 0.5)
Poisson(lambda = 3.5)
Geometric(p = 0.3)
NegativeBinomial(r = 5, p = 0.5)
HyperGeometric(N = 50, K = 10, n = 5)
```

### Distribution Methods

```scala
val dist = Normal(0.0, 1.0)

dist.pdf(x)          // Probability density/mass at x
dist.cdf(x)          // Cumulative probability P(X ≤ x)
dist.invcdf(p)       // Quantile function (inverse CDF)
dist.draw()          // Single random sample
dist.draw(n)         // Array of n random samples
dist.mean()          // Expected value
dist.variance()      // Variance
dist.stddev()        // Standard deviation
```

## Statistical Functions

### Descriptive Statistics

```scala
import breeze.stats._
import breeze.linalg._

val data = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)

mean(data)           // Arithmetic mean
median(data)         // Median
variance(data)       // Sample variance
stddev(data)         // Sample standard deviation
min(data)            // Minimum value
max(data)            // Maximum value
sum(data)            // Sum of elements
```

### Moments

```scala
import au.id.cxd.math.function.moments._

Skewness(data)       // Skewness (asymmetry)
Kurtosis(data)       // Kurtosis (tail heaviness)
Moment(data, k)      // k-th moment about the mean
```

### Correlation and Covariance

```scala
import breeze.linalg._

val x = DenseVector(1.0, 2.0, 3.0, 4.0)
val y = DenseVector(2.0, 4.0, 5.0, 4.0)

// Pearson correlation
breeze.stats.corrcoeff(x, y)

// Covariance matrix
val data = DenseMatrix(...)
breeze.stats.covmat(data)

// Correlation matrix
import au.id.cxd.math.function.column.ColCor
ColCor(data).corMatrix
```

## Linear Algebra

### Matrix Operations

```scala
import breeze.linalg._

// Creation
DenseMatrix.zeros[Double](rows, cols)
DenseMatrix.ones[Double](rows, cols)
DenseMatrix.eye[Double](n)              // Identity matrix
DenseMatrix.rand[Double](rows, cols)    // Random matrix

// Operations
A + B                // Addition
A - B                // Subtraction
A * B                // Matrix multiplication
A :* B               // Element-wise multiplication
A.t                  // Transpose
inv(A)               // Inverse
det(A)               // Determinant
trace(A)             // Trace
```

### Decompositions

```scala
import breeze.linalg._

// Eigendecomposition
val eig = eigSym(A)
eig.eigenvalues      // Eigenvalues
eig.eigenvectors     // Eigenvectors

// SVD
val svd = svd(A)
svd.U                // Left singular vectors
svd.S                // Singular values  
svd.Vt               // Right singular vectors (transposed)

// QR decomposition
val qr = qr(A)
qr.q                 // Orthogonal matrix
qr.r                 // Upper triangular matrix

// Cholesky decomposition
val chol = cholesky(A)  // For positive definite matrices
```

### Vector Operations

```scala
import breeze.linalg._

// Creation
DenseVector.zeros[Double](n)
DenseVector.ones[Double](n)
DenseVector.rand[Double](n)
DenseVector(1.0, 2.0, 3.0)  // From elements

// Operations
a + b                // Addition
a - b                // Subtraction
a :* b               // Element-wise multiplication
a dot b              // Dot product
norm(a)              // Euclidean norm
normalize(a)         // Unit vector
sum(a)               // Sum of elements
argmax(a)            // Index of maximum
argmin(a)            // Index of minimum
```

## Regression

### Linear Regression

```scala
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._

val model = LinearRegression(y, X)

model.beta                    // Coefficients
model.rSquared                // R-squared
model.adjustedRSquared        // Adjusted R-squared
model.residuals               // Residuals
model.predict                 // Fitted values
model.residualStandardError   // RSE
model.op(newX)                // Predict on new data
```

### Logistic Regression

```scala
import au.id.cxd.math.function.approximate.LogisticRegression

val model = LogisticRegression(y, X)

model.beta                    // Coefficients
model.predict(X)              // Predicted probabilities
model.classify(X, threshold)  // Classifications
```

### Bayesian Regression

```scala
import au.id.cxd.math.function.approximate.BayesianLinearRegression

val model = BayesianLinearRegression(y, X, alpha, beta)

model.mean                    // Posterior mean
model.covariance              // Posterior covariance
model.predictWithUncertainty(newX)  // (mean, variance)
```

## Neural Networks

### Building Networks

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._

val network = Builder()
  .addLayer(Linear(inputSize, hiddenSize))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(hiddenSize, outputSize))
  .addLayer(Activation(Sigmoid))
  .build()
```

### Training

```scala
import au.id.cxd.math.model.network.train.SGDTrainer
import au.id.cxd.math.model.network.loss.SquareError

val trainer = SGDTrainer(
  learningRate = 0.01,
  epochs = 1000,
  lossFunction = SquareError
)

val trained = trainer.train(network, X, y)
```

### Prediction

```scala
val predictions = network.forward(X)
val singlePred = network.forward(DenseMatrix(input))
```

## Text Processing

### Document-Term Matrix

```scala
import au.id.cxd.text.count.DocumentTermVectoriser

val docs = Seq("doc1 text", "doc2 text", "doc3 text")
val vectoriser = DocumentTermVectoriser()
val (matrix, dictionary) = vectoriser.makeDocTermMatrix(docs)
```

### TF-IDF

```scala
import au.id.cxd.text.count.TfIdfCount

val tfidf = TfIdfCount(docTermMatrix)
val tfidfMatrix = tfidf.tfidf
```

### Latent Semantic Indexing

```scala
import au.id.cxd.text.model.LatentSemanticIndex

val lsi = LatentSemanticIndex(docTermMatrix, k = 10)
val transformed = lsi.transform(docTermMatrix)
```

### Text Preprocessing

```scala
import au.id.cxd.text.preprocess._

// Stopwords removal
val stopwords = StopwordsLoader.load()
val filtered = StopwordPatternFilter(stopwords).filter(text)

// Stemming
val stemmer = PorterStemmer()
val stemmed = stemmer.stem(word)
```

## Data Processing

### Loading CSV

```scala
import au.id.cxd.math.data.CsvReader
import java.io.File

val reader = CsvReader()
val data = reader.read(new File("data.csv"))

data.matrix          // DenseMatrix of values
data.headers         // Column names
data.rowLabels       // Row labels
```

### Normalization

```scala
import au.id.cxd.math.function.transform._

// Standardization (z-score)
val normalizer = StandardisedNormalisation(data)
val standardized = normalizer.transform(data)
val original = normalizer.inverseTransform(standardized)

// Min-Max scaling
val scaler = MinMaxNormalisation(data, min = 0.0, max = 1.0)
val scaled = scaler.transform(data)

// Log transform
val logTransform = LogTransform()
val transformed = logTransform.transform(data)
```

### Data Splitting

```scala
import au.id.cxd.math.data.DataSet

// Train-test split
val (train, test) = DataSet.split(data, trainRatio = 0.8)

// K-fold cross-validation
val folds = DataSet.kfold(data, k = 5)
```

## Distance Metrics

```scala
import au.id.cxd.math.function.distance._

val x = DenseVector(1.0, 2.0, 3.0)
val y = DenseVector(4.0, 5.0, 6.0)

// Euclidean distance
Euclidean().measure(x, y)

// Manhattan distance
Manhattan().measure(x, y)

// Cosine similarity
Cosine().measure(x, y)         // Returns distance (1 - similarity)
1.0 - Cosine().measure(x, y)   // Returns similarity

// Mahalanobis distance
val covariance = DenseMatrix(...)
Mahalanobis(covariance).measure(x, y)
```

## Multivariate Analysis

### PCA

```scala
import au.id.cxd.math.model.components.PrincipalComponentsAnalysis

val pca = PrincipalComponentsAnalysis(data)

pca.eigenValues              // Eigenvalues
pca.eigenVectors             // Eigenvectors (principal components)
pca.varianceExplained        // Proportion of variance explained
pca.transform(data)          // Project data onto components
```

### Clustering

```scala
import au.id.cxd.math.model.cluster.KMeans

val kmeans = KMeans(k = 3, data)

kmeans.centers               // Cluster centers
kmeans.assignments           // Cluster assignments
kmeans.predict(newPoint)     // Predict cluster for new point
```

## Special Functions

### Gamma Functions

```scala
import au.id.cxd.math.function.gamma._

Gamma.gamma(x)               // Gamma function Γ(x)
Gamma.lnGamma(x)             // Log-gamma ln(Γ(x))
Gamma.digamma(x)             // Digamma function ψ(x)
Gamma.trigamma(x)            // Trigamma function ψ'(x)
Gamma.invGamma(p, a)         // Inverse gamma
```

### Beta Functions

```scala
import au.id.cxd.math.function.beta._

Beta.beta(a, b)              // Beta function B(a,b)
Beta.lnBeta(a, b)            // Log-beta
Beta.incompleteBeta(x, a, b) // Incomplete beta
```

### Error Functions

```scala
import au.id.cxd.math.function.erf._

Erf.erf(x)                   // Error function
Erf.erfc(x)                  // Complementary error function
Erf.erfInv(p)                // Inverse error function
```

## Constants and Utilities

```scala
import au.id.cxd.math.definitions._

// Mathematical constants
MathConstants.PI
MathConstants.E
MathConstants.GOLDEN_RATIO

// Combinatorial functions
import au.id.cxd.math.count._

Factorial.factorial(n)       // n!
Combinations.choose(n, k)    // C(n,k) = n! / (k!(n-k)!)
Permutations.permute(n, k)   // P(n,k) = n! / (n-k)!
```

## Import Summary

Common import statements:

```scala
// Core linear algebra
import breeze.linalg._
import breeze.stats._

// Probability
import au.id.cxd.math.probability.continuous._
import au.id.cxd.math.probability.discrete._

// Regression
import au.id.cxd.math.function.approximate._

// Neural networks
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._
import au.id.cxd.math.model.network.loss._
import au.id.cxd.math.model.network.train._

// Text processing
import au.id.cxd.text.model._
import au.id.cxd.text.count._
import au.id.cxd.text.preprocess._

// Data processing
import au.id.cxd.math.data._
import au.id.cxd.math.function.transform._

// Multivariate methods
import au.id.cxd.math.model.components._
import au.id.cxd.math.model.cluster._
```

## See Also

- [Getting Started](Getting-Started.md) - Installation and basics
- [Probability Distributions](Probability-Distributions.md) - Detailed distribution guide
- [Regression Methods](Regression-Methods.md) - Regression techniques
- [Neural Networks](Neural-Networks.md) - Deep learning guide
- [Examples Catalog](Examples-Catalog.md) - Working examples

---

[← Back to Home](Home.md)

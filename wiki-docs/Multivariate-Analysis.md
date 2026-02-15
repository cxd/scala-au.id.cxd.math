# Multivariate Analysis

Comprehensive guide to multivariate statistical methods for analyzing data with multiple variables.

## Overview

Multivariate analysis techniques help you understand relationships between multiple variables simultaneously. This library provides several key methods:

- **Principal Component Analysis (PCA)** - Dimensionality reduction and feature extraction
- **Discriminant Analysis** - Classification and group separation
- **K-Means Clustering** - Unsupervised grouping of observations
- **Eigendecomposition & SVD** - Matrix factorization techniques

**Package**: `au.id.cxd.math.model.components` and `au.id.cxd.math.model.cluster`

## Principal Component Analysis (PCA)

PCA is a dimensionality reduction technique that transforms data into a new coordinate system where the axes (principal components) explain maximum variance.

### Basic PCA

**Package**: `au.id.cxd.math.model.components.PrincipleComponentsAnalysis`

```scala
import au.id.cxd.math.model.components.PrincipleComponentsAnalysis
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg._

// Sample data (100 observations, 5 features)
val data = DenseMatrix.rand[Double](100, 5)

// Standardize the data (recommended)
val normalizer = StandardisedNormalisation(data)
val standardized = normalizer.transform(data)

// Perform PCA
val pca = new PrincipleComponentsAnalysis(scale = true)
val (eigenValues, eigenVectors, varExplained, projection) = pca.op(standardized)

// Examine results
println(s"Eigenvalues: $eigenValues")
println(s"Variance explained by each component: $varExplained")

// Determine number of components for 90% variance
val cumVar = varExplained.toArray.scanLeft(0.0)(_ + _).tail
val numComponents = cumVar.indexWhere(_ >= 0.90) + 1
println(s"Components needed for 90% variance: $numComponents")

// Use the projection (data in PC space)
println(s"Projected data shape: ${projection.rows} x ${projection.cols}")
```

### PCA Properties

```scala
val pca = new PrincipleComponentsAnalysis(scale = true)
val (eigenValues, eigenVectors, varExplained, projection) = pca.op(data)

// Eigenvalues (importance of each component)
println(s"Eigenvalues:\n$eigenValues")

// Eigenvectors (principal component directions)
println(s"First principal component:\n${eigenVectors(::, 0)}")

// Variance explained (proportion)
val totalVar = varExplained.toArray.sum
println(f"Total variance explained: ${totalVar * 100}%.2f%%")

// Cumulative variance
val cumulative = varExplained.toArray.scanLeft(0.0)(_ + _).tail
cumulative.zipWithIndex.foreach { case (cum, i) =>
  println(f"PC ${i + 1}: ${cum * 100}%.2f%% cumulative")
}
```

### Dimensionality Reduction

```scala
import breeze.linalg._

// Select top k components
val k = 3
val topComponents = eigenVectors(::, 0 until k)

// Project to reduced space
val reduced = standardized * topComponents

println(s"Original dimensions: ${data.rows} x ${data.cols}")
println(s"Reduced dimensions: ${reduced.rows} x ${reduced.cols}")

// Reconstruct approximation
val reconstructed = reduced * topComponents.t

// Calculate reconstruction error
val error = sum((standardized - reconstructed).map(x => x * x)) / data.size
println(f"Reconstruction error: $error%.4f")
```

### PCA for Visualization

```scala
// Reduce to 2D for visualization
val pca2D = eigenVectors(::, 0 until 2)
val projected2D = standardized * pca2D

println("2D projection for visualization:")
println(projected2D)

// Each row is now a 2D point that can be plotted
// Use breeze-viz or export for plotting
```

### Biplot: Variables in PC Space

```scala
// Variable loadings (correlation with PCs)
val loadings = eigenVectors * diag(eigenValues.map(math.sqrt))

// First two principal components
val loading1 = loadings(::, 0)
val loading2 = loadings(::, 1)

println("Variable loadings on PC1:")
println(loading1)

println("Variable loadings on PC2:")
println(loading2)

// Variables with high loadings are important for that PC
```

## Discriminant Analysis

Methods for classification and understanding group differences.

### Canonical Discriminant Analysis (LDA)

Linear Discriminant Analysis for classification and dimensionality reduction while preserving class separability.

**Package**: `au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis`

```scala
import au.id.cxd.math.model.components.CanonicalDiscriminantAnalysis
import breeze.linalg._

// Prepare data with class labels
// Each row is an observation, last column is class label
val data = DenseMatrix(
  (2.0, 3.0, 0.0),  // Observation 1, class 0
  (3.0, 4.0, 0.0),  // Observation 2, class 0
  (5.0, 6.0, 1.0),  // Observation 3, class 1
  (6.0, 7.0, 1.0)   // Observation 4, class 1
)

// Perform LDA
val lda = CanonicalDiscriminantAnalysis(data)

// Get discriminant functions
val discriminants = lda.discriminants
println(s"Discriminant functions:\n$discriminants")

// Project data onto discriminant space
val projected = lda.project(data)
println(s"Projected data:\n$projected")

// Classify new observations
val newData = DenseMatrix((4.0, 5.0))
val classification = lda.classify(newData)
println(s"Predicted class: $classification")
```

### Understanding LDA Results

```scala
val lda = CanonicalDiscriminantAnalysis(data)

// Between-class vs within-class variance
val betweenClassVar = lda.betweenClassScatter
val withinClassVar = lda.withinClassScatter

println(s"Between-class scatter:\n$betweenClassVar")
println(s"Within-class scatter:\n$withinClassVar")

// Eigenvalues indicate separation
val eigenvalues = lda.eigenvalues
println(s"Separation (eigenvalues): $eigenvalues")

// Larger eigenvalues = better class separation
```

### Quadratic Discriminant Analysis (QDA)

Allows for different covariance matrices per class (non-linear boundaries).

**Package**: `au.id.cxd.math.model.components.QuadraticDiscriminant`

```scala
import au.id.cxd.math.model.components.QuadraticDiscriminant
import breeze.linalg._

// Data with class labels (last column)
val data = DenseMatrix(
  (1.0, 2.0, 0.0),
  (1.5, 2.5, 0.0),
  (5.0, 5.0, 1.0),
  (5.5, 5.5, 1.0)
)

// Fit QDA model
val qda = QuadraticDiscriminant(data)

// Classify new observations
val newPoint = DenseMatrix((3.0, 3.5))
val prediction = qda.classify(newPoint)
println(s"QDA prediction: $prediction")

// QDA is useful when:
// - Classes have different covariance structures
// - Decision boundaries are non-linear
// - You have enough data per class
```

### Comparison: LDA vs QDA

| Aspect | LDA | QDA |
|--------|-----|-----|
| **Assumption** | Same covariance for all classes | Different covariance per class |
| **Decision boundary** | Linear | Quadratic (curved) |
| **Parameters** | Fewer | More |
| **Data requirements** | Less data needed | More data needed per class |
| **Flexibility** | Less flexible | More flexible |
| **Use when** | Classes have similar spread | Classes have different spreads |

## Clustering

Unsupervised methods for grouping similar observations.

### K-Means Clustering

Partition data into k clusters by minimizing within-cluster variance.

**Package**: `au.id.cxd.math.model.cluster.KMeans`

```scala
import au.id.cxd.math.model.cluster.KMeans
import breeze.linalg._

// Sample data
val data = DenseMatrix(
  (1.0, 2.0),
  (1.5, 1.8),
  (5.0, 8.0),
  (8.0, 8.0),
  (1.0, 0.6),
  (9.0, 11.0),
  (8.0, 2.0),
  (10.0, 2.0),
  (9.0, 3.0)
)

// Cluster into k groups
val k = 3
val kmeans = KMeans(k, data)

// Get results
val assignments = kmeans.assignments
val centers = kmeans.centers
val wcss = kmeans.withinClusterSumSquares

println(s"Cluster assignments: $assignments")
println(s"Cluster centers:\n$centers")
println(f"Within-cluster sum of squares: $wcss%.2f")

// Predict cluster for new points
val newPoint = DenseVector(2.0, 3.0)
val cluster = kmeans.predict(newPoint)
println(s"New point assigned to cluster: $cluster")
```

### K-Means Parameters

```scala
// Different initializations
val kmeans1 = KMeans(k = 3, data, maxIterations = 100)
val kmeans2 = KMeans(k = 3, data, maxIterations = 300)

// Check convergence
println(s"Converged in ${kmeans1.iterations} iterations")
println(s"Final WCSS: ${kmeans1.withinClusterSumSquares}")
```

### Choosing K: Elbow Method

```scala
import breeze.linalg._

// Try different values of k
val kValues = 2 to 10
val wcssValues = kValues.map { k =>
  val kmeans = KMeans(k, data)
  (k, kmeans.withinClusterSumSquares)
}

println("K vs WCSS:")
wcssValues.foreach { case (k, wcss) =>
  println(f"k=$k: WCSS=$wcss%.2f")
}

// Look for "elbow" - point where WCSS decreases slowly
// Plot these values to visualize
```

### Cluster Analysis

```scala
val kmeans = KMeans(k = 3, data)

// Analyze cluster sizes
val clusterSizes = (0 until k).map { i =>
  kmeans.assignments.count(_ == i)
}
println(s"Cluster sizes: ${clusterSizes.mkString(", ")}")

// Cluster statistics
val clusterData = (0 until k).map { i =>
  val clusterPoints = data(kmeans.assignments.zipWithIndex
    .filter(_._1 == i)
    .map(_._2), ::)
  
  println(s"\nCluster $i statistics:")
  println(s"  Size: ${clusterSizes(i)}")
  println(s"  Center: ${kmeans.centers(i, ::)}")
}
```

### Hierarchical Clustering

While not directly implemented, you can build hierarchical clustering:

```scala
import au.id.cxd.math.function.distance.Euclidean

// Distance matrix
def computeDistanceMatrix(data: DenseMatrix[Double]): DenseMatrix[Double] = {
  val n = data.rows
  val distances = DenseMatrix.zeros[Double](n, n)
  val euclidean = Euclidean()
  
  for (i <- 0 until n; j <- i + 1 until n) {
    val dist = euclidean.measure(data(i, ::).t, data(j, ::).t)
    distances(i, j) = dist
    distances(j, i) = dist
  }
  distances
}

val distMatrix = computeDistanceMatrix(data)
println("Distance matrix:")
println(distMatrix)
```

## Matrix Decomposition Methods

### Eigendecomposition

Decompose a square matrix into eigenvalues and eigenvectors.

**Package**: `au.id.cxd.math.model.components.EigenDecomposition`

```scala
import au.id.cxd.math.model.components.EigenDecomposition
import breeze.linalg._

// Symmetric matrix (e.g., covariance matrix)
val matrix = DenseMatrix(
  (4.0, 2.0, 1.0),
  (2.0, 5.0, 3.0),
  (1.0, 3.0, 6.0)
)

// Eigendecomposition
val (eigenValues, eigenVectors, varExplained) = EigenDecomposition(matrix)

println(s"Eigenvalues: $eigenValues")
println(s"Eigenvectors:\n$eigenVectors")
println(s"Variance explained: $varExplained")

// Verify: A * v = λ * v
val v = eigenVectors(::, 0)
val lambda = eigenValues(0)
val Av = matrix * v
val lambdaV = v * lambda

println(s"Av:\n$Av")
println(s"λv:\n$lambdaV")
println(s"Close? ${max(abs(Av - lambdaV)) < 1e-10}")
```

### Singular Value Decomposition (SVD)

Decompose any matrix into three matrices: U * Σ * V^T.

**Package**: `au.id.cxd.math.model.components.SingularValueDecomposition`

```scala
import au.id.cxd.math.model.components.SingularValueDecomposition
import breeze.linalg._

// Any matrix (doesn't need to be square)
val matrix = DenseMatrix(
  (1.0, 2.0, 3.0),
  (4.0, 5.0, 6.0),
  (7.0, 8.0, 9.0),
  (10.0, 11.0, 12.0)
)

// Perform SVD
val svd = SingularValueDecomposition(matrix)

println(s"U (left singular vectors):\n${svd.U}")
println(s"Σ (singular values): ${svd.S}")
println(s"V (right singular vectors):\n${svd.V}")

// Reconstruct matrix
val reconstructed = svd.U * diag(svd.S) * svd.V.t
println(s"Reconstruction error: ${max(abs(matrix - reconstructed))}")

// Low-rank approximation
val k = 2
val approx = svd.U(::, 0 until k) * diag(svd.S(0 until k)) * svd.V(::, 0 until k).t
println(s"Rank-$k approximation:\n$approx")
```

### SVD Applications

```scala
// 1. Dimensionality reduction (similar to PCA)
val reducedU = svd.U(::, 0 until 2)
val reducedS = svd.S(0 until 2)
val projection = reducedU * diag(reducedS)

// 2. Pseudoinverse (for solving least squares)
val pseudoInverse = svd.V * diag(svd.S.map(s => 1.0 / s)) * svd.U.t

// 3. Matrix rank
val tolerance = 1e-10
val rank = svd.S.count(_ > tolerance)
println(s"Matrix rank: $rank")

// 4. Condition number
val conditionNumber = svd.S(0) / svd.S(svd.S.length - 1)
println(f"Condition number: $conditionNumber%.2f")
```

## Complete Workflow Example

### Multivariate Analysis Pipeline

```scala
import au.id.cxd.math.model.components._
import au.id.cxd.math.model.cluster.KMeans
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg._

// 1. Load and prepare data
val rawData = DenseMatrix.rand[Double](200, 8)  // 200 samples, 8 features

// 2. Standardize
val normalizer = StandardisedNormalisation(rawData)
val data = normalizer.transform(rawData)

// 3. PCA for dimensionality reduction
val pca = new PrincipleComponentsAnalysis(scale = false)  // Already scaled
val (eigenVals, eigenVecs, varExp, projection) = pca.op(data)

println(s"Variance explained: ${varExp.toArray.take(5).mkString(", ")}")

// 4. Select components (95% variance)
val cumVar = varExp.toArray.scanLeft(0.0)(_ + _).tail
val numComponents = cumVar.indexWhere(_ >= 0.95) + 1
val reducedData = projection(::, 0 until numComponents)

println(s"Reduced from ${data.cols} to $numComponents dimensions")

// 5. Cluster in reduced space
val kmeans = KMeans(k = 3, reducedData)

println(s"Cluster assignments: ${kmeans.assignments.take(10).mkString(", ")}")
println(s"Cluster centers:\n${kmeans.centers}")

// 6. Analyze results
val clusterSizes = (0 until 3).map(i => kmeans.assignments.count(_ == i))
println(s"Cluster sizes: ${clusterSizes.mkString(", ")}")
```

## Best Practices

### Data Preprocessing

1. **Standardize/normalize** before PCA or discriminant analysis
2. **Remove outliers** that might distort analysis
3. **Handle missing values** appropriately
4. **Check for multicollinearity** in features

### Choosing Methods

| Goal | Method | When to Use |
|------|--------|-------------|
| **Reduce dimensions** | PCA | Unsupervised, keep variance |
| **Classify with labels** | LDA | Linear boundaries, labeled data |
| **Classify (non-linear)** | QDA | Non-linear boundaries, enough data |
| **Find groups** | K-Means | Unsupervised, spherical clusters |
| **Understand structure** | SVD | Any matrix, flexible |

### Validation

```scala
// PCA: Check cumulative variance
val cumVar = varExplained.toArray.scanLeft(0.0)(_ + _).tail
println("Cumulative variance:")
cumVar.take(5).zipWithIndex.foreach { case (v, i) =>
  println(f"  ${i + 1} components: ${v * 100}%.2f%%")
}

// K-Means: Try multiple k values
val wcssValues = (2 to 10).map { k =>
  val km = KMeans(k, data)
  (k, km.withinClusterSumSquares)
}
println("Elbow plot data:")
wcssValues.foreach { case (k, wcss) =>
  println(f"  k=$k: WCSS=$wcss%.2f")
}

// LDA: Check classification accuracy
// Use cross-validation or holdout set
```

## Common Pitfalls

- ❌ Not standardizing data before PCA
- ❌ Using too many/too few components
- ❌ Interpreting loadings without scaling
- ❌ Using K-Means on non-spherical clusters
- ❌ Not validating number of clusters
- ❌ Applying LDA when assumptions violated

## See Also

- [Statistical Tests](Statistical-Tests.md) - MANOVA, multivariate normality
- [Data Processing](Data-Processing.md) - Standardization and preprocessing
- [Examples Catalog](Examples-Catalog.md) - Working examples
- [API Quick Reference](API-Quick-Reference.md) - Quick syntax lookup

---

[← Back to Home](Home.md)

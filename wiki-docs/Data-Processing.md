# Data Processing

Comprehensive guide to data loading, preprocessing, transformation, and dataset management.

## Overview

The library provides tools for handling data throughout the machine learning pipeline:

- **Data Loading** - CSV readers and various data formats
- **Data Normalization** - Standardization and scaling techniques
- **Data Transformation** - Various transformations for preprocessing
- **Data Partitioning** - Train/validation/test splits
- **Batch Processing** - Working with data in batches
- **Dataset Management** - Organizing and managing datasets

**Package**: `au.id.cxd.math.data` and `au.id.cxd.math.function.transform`

## Quick Start

### Basic Data Pipeline

```scala
import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.data.Partition
import java.io.File

// 1. Load data
val reader = new CsvReader()
val dataset = reader.read(new File("data.csv"))

// 2. Normalize
val normalizer = StandardisedNormalisation()
val normalized = normalizer.transform(dataset.matrix)

// 3. Split into train/validation/test
val Seq((trainData, valData, testData)) = Partition(
  Seq(normalized),
  train = 0.7,
  valid = 0.15,
  test = 0.15
)

println(s"Train: ${trainData.rows} rows")
println(s"Validation: ${valData.rows} rows")
println(s"Test: ${testData.rows} rows")
```

## Data Loading

### CSV Reader

**Package**: `au.id.cxd.math.data.CsvReader`

The CSV reader provides flexible data loading from comma-separated or tab-separated files.

```scala
import au.id.cxd.math.data.CsvReader
import java.io.File

// Create reader (default separators: tab and comma)
val reader = new CsvReader()

// Read CSV file
val file = new File("dataset.csv")
val dataSet = reader.read(file)

// Access components
val matrix = dataSet.matrix          // DenseMatrix[Double] - numeric data
val headers = dataSet.headers        // Seq[String] - column names
val rowLabels = dataSet.rowLabels    // Seq[String] - row identifiers (if present)

println(s"Loaded ${matrix.rows} rows x ${matrix.cols} columns")
println(s"Headers: ${headers.mkString(", ")}")
```

### Custom Separators

```scala
// Use custom separators
val tabReader = new CsvReader(Array('\t'))  // Tab-separated
val commaReader = new CsvReader(Array(','))  // Comma-separated
val customReader = new CsvReader(Array('|', ';'))  // Multiple separators

val data = tabReader.read(new File("data.tsv"))
```

### Reading with Headers

```scala
val reader = new CsvReader()
val dataSet = reader.read(new File("data_with_headers.csv"))

// First row is treated as headers
println("Column names:")
dataSet.headers.zipWithIndex.foreach { case (name, idx) =>
  println(s"  Column $idx: $name")
}

// Access data by column
val col0 = dataSet.matrix(::, 0)
println(s"Column '${dataSet.headers(0)}': $col0")
```

### Handling Missing Values

```scala
import breeze.linalg._

val reader = new CsvReader()
val dataSet = reader.read(new File("data_with_missing.csv"))

// Check for NaN values
val hasNaN = dataSet.matrix.toArray.exists(_.isNaN)
println(s"Contains missing values: $hasNaN")

// Remove rows with NaN
val cleanRows = (0 until dataSet.matrix.rows).filter { i =>
  !dataSet.matrix(i, ::).t.toArray.exists(_.isNaN)
}
val cleanData = dataSet.matrix(cleanRows, ::).toDenseMatrix

println(s"Original rows: ${dataSet.matrix.rows}")
println(s"Clean rows: ${cleanData.rows}")

// Fill NaN with column mean
val filledData = DenseMatrix.tabulate(dataSet.matrix.rows, dataSet.matrix.cols) { 
  case (i, j) =>
    val value = dataSet.matrix(i, j)
    if (value.isNaN) {
      // Compute column mean excluding NaN
      val col = dataSet.matrix(::, j)
      val validValues = col.toArray.filter(!_.isNaN)
      validValues.sum / validValues.length
    } else value
}
```

## Data Normalization

Normalization techniques for preparing data for analysis.

### Standardization (Z-Score Normalization)

**Package**: `au.id.cxd.math.function.transform.StandardisedNormalisation`

Transform data to have mean 0 and variance 1:

$$
Z = \frac{X - \mu}{\sigma}
$$

```scala
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 2.0, 3.0),
  (4.0, 5.0, 6.0),
  (7.0, 8.0, 9.0),
  (10.0, 11.0, 12.0)
)

// Create normalizer
val normalizer = StandardisedNormalisation()

// Transform data
val normalized = normalizer.transform(data)

println("Normalized data:")
println(normalized)

// Verify mean ≈ 0 and variance ≈ 1
val col0 = normalized(::, 0)
val mean = breeze.stats.mean(col0)
val variance = breeze.stats.variance(col0)

println(f"Column 0 - Mean: $mean%.6f, Variance: $variance%.6f")
```

### Inverse Transform

```scala
val normalizer = StandardisedNormalisation()
val normalized = normalizer.transform(data)

// Transform back to original scale
val reconstructed = normalizer.invert(normalized)

println("Original data:")
println(data)
println("\nReconstructed data:")
println(reconstructed)

// Verify reconstruction
val maxDiff = breeze.linalg.max(breeze.linalg.abs(data - reconstructed))
println(f"Max difference: $maxDiff%.10f")
```

### Using Pre-computed Parameters

```scala
// First transformation computes parameters
val normalizer = StandardisedNormalisation()
val normalized1 = normalizer.transform(trainingData)

// Store parameters
val meanVector = normalizer.meanVector
val sigmaVector = normalizer.sigmaVector

println(s"Stored mean: $meanVector")
println(s"Stored variance: $sigmaVector")

// Apply same transformation to new data
val normalized2 = normalizer.filter(testData)

// This ensures test data uses training statistics
```

### Min-Max Normalization

**Package**: `au.id.cxd.math.function.transform.MinMaxNormalisation`

Scale data to a specific range [min, max]:

```scala
import au.id.cxd.math.function.transform.MinMaxNormalisation
import breeze.linalg._

val data = DenseMatrix(
  (1.0, 10.0),
  (2.0, 20.0),
  (3.0, 30.0),
  (4.0, 40.0)
)

// Scale to [0, 1] range (default)
val scaler = MinMaxNormalisation(data)
val scaled = scaler.transform(data)

println("Scaled to [0, 1]:")
println(scaled)

// Verify range
println(f"Min: ${breeze.linalg.min(scaled)}%.4f")
println(f"Max: ${breeze.linalg.max(scaled)}%.4f")

// Scale to custom range [-1, 1]
val customScaler = MinMaxNormalisation(data, min = -1.0, max = 1.0)
val customScaled = customScaler.transform(data)

println("\nScaled to [-1, 1]:")
println(customScaled)
```

### Inverse Min-Max Transform

```scala
val scaler = MinMaxNormalisation(data)
val scaled = scaler.transform(data)

// Transform back to original scale
val reconstructed = scaler.invert(scaled)

println("Original data:")
println(data)
println("\nReconstructed:")
println(reconstructed)
```

## Data Transformations

Additional transformation techniques.

### Identity Transform

**Package**: `au.id.cxd.math.function.transform.IdentityTransform`

Pass-through transformation (no change):

```scala
import au.id.cxd.math.function.transform.IdentityTransform

val identity = IdentityTransform()
val result = identity.transform(data)

// result == data (no transformation applied)
```

### Logarithmic Transform

Useful for data with exponential growth or right-skewed distributions:

```scala
import breeze.linalg._
import breeze.numerics._

val data = DenseMatrix(
  (1.0, 10.0, 100.0),
  (2.0, 20.0, 200.0),
  (3.0, 30.0, 300.0)
)

// Natural log transform
val logTransformed = log(data)

println("Log-transformed data:")
println(logTransformed)

// Log10 transform
val log10Transformed = log10(data)

// Inverse: exponential
val reconstructed = exp(logTransformed)
```

### Power Transform

```scala
import breeze.linalg._
import breeze.numerics._

// Square root transform (useful for count data)
val sqrtTransformed = sqrt(data)

// Square transform
val squareTransformed = data.map(x => x * x)

// General power transform
val power = 0.5
val powerTransformed = data.map(x => math.pow(x, power))
```

### Box-Cox Transform

For making data more normal-like:

```scala
def boxCoxTransform(data: DenseMatrix[Double], lambda: Double): DenseMatrix[Double] = {
  data.map { x =>
    if (math.abs(lambda) < 1e-10) {
      math.log(x)
    } else {
      (math.pow(x, lambda) - 1) / lambda
    }
  }
}

val lambda = 0.5
val transformed = boxCoxTransform(data, lambda)
```

## Data Partitioning

Split data into training, validation, and test sets.

### Basic Train/Test Split

**Package**: `au.id.cxd.math.data.Partition`

```scala
import au.id.cxd.math.data.Partition
import breeze.linalg._

val data = DenseMatrix.rand[Double](100, 5)  // 100 samples, 5 features

// Split: 70% train, 15% validation, 15% test
val Seq((trainData, valData, testData)) = Partition(
  Seq(data),
  train = 0.7,
  valid = 0.15,
  test = 0.15
)

println(s"Training set: ${trainData.rows} samples")
println(s"Validation set: ${valData.rows} samples")
println(s"Test set: ${testData.rows} samples")
```

### Multiple Datasets Split

```scala
val X = DenseMatrix.rand[Double](100, 5)
val y = DenseMatrix.rand[Double](100, 1)

// Split both X and y with same indices
val partitions = Partition(
  Seq(X, y),
  train = 0.7,
  valid = 0.15,
  test = 0.15
)

val (X_train, X_val, X_test) = partitions(0)
val (y_train, y_val, y_test) = partitions(1)

println(s"X_train shape: ${X_train.rows} x ${X_train.cols}")
println(s"y_train shape: ${y_train.rows} x ${y_train.cols}")
```

### Custom Split Ratios

```scala
// 80% training, 10% validation, 10% test
val Seq((train1, val1, test1)) = Partition(
  Seq(data),
  train = 0.8,
  valid = 0.1,
  test = 0.1
)

// 60% training, 20% validation, 20% test
val Seq((train2, val2, test2)) = Partition(
  Seq(data),
  train = 0.6,
  valid = 0.2,
  test = 0.2
)
```

### Random Shuffling

```scala
import breeze.linalg._
import scala.util.Random

// Shuffle data before splitting
def shuffleData(data: DenseMatrix[Double]): DenseMatrix[Double] = {
  val indices = Random.shuffle((0 until data.rows).toList)
  data(indices, ::).toDenseMatrix
}

val shuffled = shuffleData(data)
val Seq((trainData, valData, testData)) = Partition(
  Seq(shuffled),
  train = 0.7,
  valid = 0.15,
  test = 0.15
)
```

### K-Fold Cross-Validation

```scala
import breeze.linalg._

def kFoldSplit(data: DenseMatrix[Double], k: Int): Seq[(DenseMatrix[Double], DenseMatrix[Double])] = {
  val n = data.rows
  val foldSize = n / k
  
  (0 until k).map { fold =>
    val testStart = fold * foldSize
    val testEnd = if (fold == k - 1) n else (fold + 1) * foldSize
    
    val trainIndices = (0 until testStart) ++ (testEnd until n)
    val testIndices = testStart until testEnd
    
    val train = data(trainIndices, ::).toDenseMatrix
    val test = data(testIndices, ::).toDenseMatrix
    
    (train, test)
  }
}

// Create 5-fold splits
val folds = kFoldSplit(data, k = 5)

folds.zipWithIndex.foreach { case ((train, test), i) =>
  println(s"Fold $i: train=${train.rows} rows, test=${test.rows} rows")
}
```

## Batch Processing

Process data in batches for memory efficiency.

### Creating Batches

**Package**: `au.id.cxd.math.data.Batch`

```scala
import au.id.cxd.math.data.Batch
import breeze.linalg._

val data = DenseMatrix.rand[Double](1000, 10)  // Large dataset
val batchSize = 32

// Process in batches
val numBatches = math.ceil(data.rows.toDouble / batchSize).toInt

for (i <- 0 until numBatches) {
  val startIdx = i * batchSize
  val endIdx = math.min((i + 1) * batchSize, data.rows)
  
  val batch = data(startIdx until endIdx, ::).toDenseMatrix
  
  println(s"Batch $i: ${batch.rows} samples")
  
  // Process batch
  // ... your processing code here
}
```

### Batch Iterator

```scala
def batchIterator(data: DenseMatrix[Double], 
                 batchSize: Int): Iterator[DenseMatrix[Double]] = {
  (0 until data.rows by batchSize).iterator.map { startIdx =>
    val endIdx = math.min(startIdx + batchSize, data.rows)
    data(startIdx until endIdx, ::).toDenseMatrix
  }
}

// Use iterator
val batches = batchIterator(data, batchSize = 64)

batches.zipWithIndex.foreach { case (batch, idx) =>
  println(s"Processing batch $idx with ${batch.rows} samples")
  // Process batch
}
```

### Mini-Batch Gradient Descent

```scala
import breeze.linalg._

def miniBatchGD(X: DenseMatrix[Double],
               y: DenseVector[Double],
               batchSize: Int,
               epochs: Int,
               learningRate: Double): DenseVector[Double] = {
  
  var weights = DenseVector.zeros[Double](X.cols)
  
  for (epoch <- 0 until epochs) {
    // Shuffle data
    val indices = scala.util.Random.shuffle((0 until X.rows).toList)
    
    // Process batches
    for (i <- indices.grouped(batchSize)) {
      val X_batch = X(i, ::).toDenseMatrix
      val y_batch = y(i).toDenseVector
      
      // Compute gradient and update
      val predictions = X_batch * weights
      val errors = predictions - y_batch
      val gradient = (X_batch.t * errors) / i.length.toDouble
      
      weights -= gradient * learningRate
    }
  }
  
  weights
}
```

## Dataset Management

Organize and manage datasets.

### DataSet Class

**Package**: `au.id.cxd.math.data.DataSet`

```scala
import au.id.cxd.math.data.DataSet
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg._

// Create dataset with continuous and discrete columns
val data = DenseMatrix.rand[Double](100, 8)
val continuousCols = 6  // First 6 columns are continuous
val discreteCols = 2    // Last 2 columns are discrete/categorical

val transform = StandardisedNormalisation()
val discreteMapping = Map(
  "category1" -> Set("A", "B", "C"),
  "category2" -> Set("X", "Y")
)

val dataset = new DataSet(
  data,
  continuousCols,
  discreteCols,
  discreteMapping,
  transform
)

println(s"Dataset: ${dataset.data.rows} rows x ${dataset.data.cols} columns")
println(s"Continuous columns: ${dataset.continuousCols}")
println(s"Discrete columns: ${dataset.discreteCols}")
```

### Working with Features

```scala
val dataset = new DataSet(data, 6, 2, discreteMapping, transform)

// Access continuous features
val continuousData = dataset.data(::, 0 until dataset.continuousCols)

// Access discrete/categorical features  
val discreteData = dataset.data(::, dataset.continuousCols until dataset.data.cols)

println(s"Continuous features: ${continuousData.cols} columns")
println(s"Discrete features: ${discreteData.cols} columns")
```

### Dummy Variable Encoding

**Package**: `au.id.cxd.math.data.DummyVariableBuilder`

```scala
import au.id.cxd.math.data.DummyVariableBuilder
import breeze.linalg._

// Categorical variable with 3 classes
val categories = Seq("red", "green", "blue", "red", "green", "blue", "red")

// Create dummy variables (one-hot encoding)
val builder = DummyVariableBuilder()
val (encoded, mapping) = builder.build(categories)

println("Encoded matrix:")
println(encoded)
println(s"\nMapping: $mapping")

// Result is a matrix with binary columns for each category
// Each row has a 1 in the column corresponding to its category
```

## Complete Workflow

### End-to-End Data Pipeline

```scala
import au.id.cxd.math.data.{CsvReader, Partition}
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.function.approximate.LinearRegression
import breeze.linalg._
import java.io.File

// 1. Load data
println("Step 1: Loading data...")
val reader = new CsvReader()
val dataset = reader.read(new File("housing_data.csv"))

println(s"Loaded ${dataset.matrix.rows} samples with ${dataset.matrix.cols} features")
println(s"Headers: ${dataset.headers.mkString(", ")}")

// 2. Separate features and target
val X = dataset.matrix(::, 0 until dataset.matrix.cols - 1)
val y = dataset.matrix(::, dataset.matrix.cols - 1)

println(s"\nFeatures: ${X.cols} columns")
println(s"Target: ${y.length} values")

// 3. Check for missing values
val hasMissing = X.toArray.exists(_.isNaN) || y.toArray.exists(_.isNaN)
if (hasMissing) {
  println("Warning: Dataset contains missing values")
}

// 4. Normalize features
println("\nStep 2: Normalizing features...")
val normalizer = StandardisedNormalisation()
val X_normalized = normalizer.transform(X)

// Verify normalization
val means = (0 until X_normalized.cols).map { col =>
  breeze.stats.mean(X_normalized(::, col))
}
println(f"Feature means after normalization: ${means.map(m => f"$m%.6f").mkString(", ")}")

// 5. Split data
println("\nStep 3: Splitting data...")
val Seq((X_train, X_val, X_test)) = Partition(
  Seq(X_normalized),
  train = 0.7,
  valid = 0.15,
  test = 0.15
)
val Seq((y_train, y_val, y_test)) = Partition(
  Seq(y.asDenseMatrix.t),
  train = 0.7,
  valid = 0.15,
  test = 0.15
)

println(s"Training: ${X_train.rows} samples")
println(s"Validation: ${X_val.rows} samples")
println(s"Test: ${X_test.rows} samples")

// 6. Train model
println("\nStep 4: Training model...")
val model = LinearRegression(y_train(::, 0), X_train)

println(f"Model trained - R²: ${model.rSquared}%.4f")

// 7. Evaluate on validation set
println("\nStep 5: Evaluating on validation set...")
val y_val_pred = model.op(X_val)
val val_mse = breeze.stats.mean((y_val_pred - y_val(::, 0)).map(x => x * x))
val val_rmse = math.sqrt(val_mse)

println(f"Validation RMSE: $val_rmse%.4f")

// 8. Test on holdout set
println("\nStep 6: Testing on holdout set...")
val y_test_pred = model.op(X_test)
val test_mse = breeze.stats.mean((y_test_pred - y_test(::, 0)).map(x => x * x))
val test_rmse = math.sqrt(test_mse)

println(f"Test RMSE: $test_rmse%.4f")

// 9. Make predictions on new data
println("\nStep 7: Making predictions on new data...")
val newSample = DenseMatrix.rand[Double](1, X.cols)
val newSample_normalized = normalizer.filter(newSample)
val prediction = model.op(newSample_normalized)

println(f"Prediction: ${prediction(0)}%.2f")
```

## Best Practices

### Data Preprocessing Checklist

1. **Load and inspect data**
   - Check dimensions
   - Examine column types
   - Look at first few rows

2. **Handle missing values**
   - Remove rows/columns with too many missing
   - Impute with mean/median/mode
   - Use model-based imputation

3. **Handle outliers**
   - Detect using IQR or z-scores
   - Remove or cap extreme values
   - Use robust scaling methods

4. **Normalize/scale features**
   - Standardization for algorithms sensitive to scale
   - Min-max for bounded ranges
   - Log transform for skewed data

5. **Encode categorical variables**
   - One-hot encoding for nominal categories
   - Ordinal encoding for ordered categories
   - Target encoding for high-cardinality

6. **Split data properly**
   - Shuffle before splitting
   - Stratify for imbalanced classes
   - Use separate test set

7. **Apply same transformations**
   - Fit on training data only
   - Transform validation/test with training parameters
   - Save transformers for production

### Data Quality Checks

```scala
import breeze.linalg._
import breeze.stats._

def dataQualityReport(data: DenseMatrix[Double], 
                     headers: Seq[String]): Unit = {
  println("Data Quality Report")
  println("=" * 50)
  
  println(s"Shape: ${data.rows} rows x ${data.cols} columns")
  println()
  
  headers.zipWithIndex.foreach { case (name, col) =>
    val column = data(::, col)
    
    val missing = column.toArray.count(_.isNaN)
    val valid = column.toArray.filterNot(_.isNaN)
    
    if (valid.nonEmpty) {
      val colMean = valid.sum / valid.length
      val colStd = math.sqrt(valid.map(x => (x - colMean) * (x - colMean)).sum / (valid.length - 1))
      val colMin = valid.min
      val colMax = valid.max
      
      println(f"Column: $name")
      println(f"  Missing: $missing (${missing * 100.0 / data.rows}%.1f%%)")
      println(f"  Mean: $colMean%.4f")
      println(f"  Std: $colStd%.4f")
      println(f"  Min: $colMin%.4f")
      println(f"  Max: $colMax%.4f")
      println()
    }
  }
}
```

### Outlier Detection

```scala
def detectOutliers(data: DenseVector[Double], 
                  method: String = "iqr"): Seq[Int] = {
  method match {
    case "iqr" =>
      val sorted = data.toArray.sorted
      val q1 = sorted((sorted.length * 0.25).toInt)
      val q3 = sorted((sorted.length * 0.75).toInt)
      val iqr = q3 - q1
      val lowerBound = q1 - 1.5 * iqr
      val upperBound = q3 + 1.5 * iqr
      
      data.toArray.zipWithIndex.filter { case (v, _) =>
        v < lowerBound || v > upperBound
      }.map(_._2)
      
    case "zscore" =>
      val mean = breeze.stats.mean(data)
      val std = breeze.stats.stddev(data)
      
      data.toArray.zipWithIndex.filter { case (v, _) =>
        math.abs((v - mean) / std) > 3.0
      }.map(_._2)
  }
}

// Use outlier detection
val column = data(::, 0)
val outlierIndices = detectOutliers(column, method = "iqr")
println(s"Found ${outlierIndices.length} outliers")
```

## Common Pitfalls

- ❌ Not splitting data before normalization
- ❌ Fitting scaler on test data
- ❌ Not handling missing values
- ❌ Ignoring data types (continuous vs categorical)
- ❌ Not shuffling data before splitting
- ❌ Leaking information from test set
- ❌ Not checking for outliers

## See Also

- [Multivariate Analysis](Multivariate-Analysis.md) - PCA and dimensionality reduction
- [Regression Methods](Regression-Methods.md) - Using processed data for regression
- [Neural Networks](Neural-Networks.md) - Deep learning with preprocessed data
- [Text Processing](Text-Processing.md) - Text-specific preprocessing
- [API Quick Reference](API-Quick-Reference.md) - Quick syntax lookup

---

[← Back to Home](Home.md)

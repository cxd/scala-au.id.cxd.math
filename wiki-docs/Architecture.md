# Architecture Overview

Understanding the design and architecture of the scala-au.id.cxd.math library.

## Project Goals

This library is designed to:

1. **Experiment with statistical methods** - Implement various algorithms to understand them deeply
2. **Learn probability theory** - Practical implementation of statistical concepts
3. **Provide educational value** - Help others learn through working examples
4. **Explore Scala patterns** - Experiment with functional programming and library design

**Not designed for**:
- Production use with strict accuracy requirements
- High-performance computing scenarios
- Mission-critical applications

## Overall Architecture

### Module Organization

```
scala-au.id.cxd.math/
│
├── math/                      # Core mathematical library
│   ├── probability/           # Probability distributions
│   ├── function/              # Mathematical functions
│   ├── model/                 # Machine learning models
│   ├── data/                  # Data I/O and management
│   ├── count/                 # Combinatorics
│   └── collection/            # Data structures
│
├── examples/                  # Example programs and tutorials
│
└── app/                       # Swing UI application
```

### Dependency Graph

```
app  ──┐
       ├──> math (core library)
examples ─┘
```

The `math` module is the core library with no dependencies on `app` or `examples`.

## Core Design Patterns

### 1. Trait-Based Polymorphism

Distributions use trait hierarchies:

```scala
trait Distribution {
  def pdf(x: Double): Double
  def cdf(x: Double): Double
  def invcdf(p: Double): Double
  def draw(): Double
}

trait ContinuousDistribution extends Distribution
trait DiscreteDistribution extends Distribution

case class Normal(mu: Double, sigma: Double) extends ContinuousDistribution
case class Binomial(n: Int, p: Double) extends DiscreteDistribution
```

**Benefits**:
- Polymorphic operations on distributions
- Easy to add new distributions
- Type-safe distinction between continuous and discrete

### 2. Case Classes for Immutability

Most classes are implemented as case classes:

```scala
case class LinearRegression(y: DenseVector[Double], X: DenseMatrix[Double]) {
  // Immutable model
  val beta: DenseVector[Double] = solve(X, y)
  val predict: DenseVector[Double] = X * beta
}
```

**Benefits**:
- Immutable by default
- Automatic `equals`, `hashCode`, `toString`
- Pattern matching support
- Thread-safe

### 3. Builder Pattern for Complex Objects

Neural networks use builder pattern:

```scala
val network = Builder()
  .addLayer(Linear(10, 20))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(20, 1))
  .build()
```

**Benefits**:
- Fluent, readable API
- Flexible construction
- Validation at build time
- Type-safe layer composition

### 4. Type Classes for Operations

Operations like distance metrics use type classes:

```scala
trait DistanceMetric {
  def measure(x: DenseVector[Double], y: DenseVector[Double]): Double
}

class Euclidean extends DistanceMetric {
  def measure(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    norm(x - y)
  }
}
```

**Benefits**:
- Extensible without modifying existing code
- Dependency injection
- Testable with mocks

### 5. Companion Objects for Factories

```scala
object Normal {
  def apply(mu: Double = 0.0, sigma: Double = 1.0): Normal = {
    require(sigma > 0, "Sigma must be positive")
    new Normal(mu, sigma)
  }
  
  def standard: Normal = Normal(0.0, 1.0)
}
```

**Benefits**:
- Default parameters
- Validation
- Alternative constructors
- Hidden implementation details

## Key Components

### Probability Module

**Structure**:
```
probability/
├── Distribution.scala          # Base trait
├── continuous/                 # Continuous distributions
│   ├── Normal.scala
│   ├── Exponential.scala
│   └── ...
├── discrete/                   # Discrete distributions
│   ├── Binomial.scala
│   ├── Poisson.scala
│   └── ...
└── random/                     # Random deviates
    ├── RNormal.scala
    └── ...
```

**Design decisions**:
- Each distribution is self-contained
- PDF, CDF, inverse CDF, and random generation in one place
- Uses special functions from `function/` module

### Function Module

**Structure**:
```
function/
├── approximate/               # Regression and approximation
├── anova/                     # ANOVA tests
├── column/                    # Column operations
├── distance/                  # Distance metrics
├── moments/                   # Statistical moments
├── transform/                 # Data transformations
└── ...
```

**Design decisions**:
- Stateless functions where possible
- Functional composition
- Immutable transformations

### Model Module

**Structure**:
```
model/
├── network/                   # Neural networks
│   ├── builder/               # Network construction
│   ├── activation/            # Activation functions
│   ├── loss/                  # Loss functions
│   └── train/                 # Training algorithms
├── components/                # PCA, factor analysis
├── cluster/                   # Clustering algorithms
└── ...
```

**Design decisions**:
- Modular layer system
- Composable training algorithms
- Separates model structure from training

## Data Flow

### Typical Workflow

```
Load Data → Preprocess → Train Model → Evaluate → Predict
```

### Example Data Flow

```scala
// 1. Load
val data = CsvReader().read("data.csv")

// 2. Preprocess
val normalizer = StandardisedNormalisation(data.matrix)
val normalized = normalizer.transform(data.matrix)

// 3. Split
val (X, y) = split(normalized)

// 4. Train
val model = LinearRegression(y, X)

// 5. Evaluate
val predictions = model.predict
val rmse = math.sqrt(mean((predictions - y).map(x => x * x)))

// 6. Predict
val newData = normalizer.transform(newObservations)
val newPredictions = model.op(newData)
```

## External Dependencies

### Core Dependencies

```scala
// Breeze - Linear algebra and numerical computing
"org.scalanlp" %% "breeze" % "0.13.2"
"org.scalanlp" %% "breeze-natives" % "0.13.2"

// Scalaz - Functional programming utilities
"org.scalaz" %% "scalaz-core" % "7.2.27"

// JSON4S - JSON serialization
"org.json4s" %% "json4s-jackson" % "3.6.0-M2"
```

### Why These Dependencies?

- **Breeze**: Industry-standard linear algebra for Scala, similar to NumPy
- **Breeze-natives**: Native BLAS/LAPACK for performance
- **Scalaz**: Functional abstractions (Option, Either, etc.)
- **JSON4S**: Model serialization

## Performance Considerations

### Breeze Integration

```scala
import breeze.linalg._

// Efficient operations
val result = A * B           // Matrix multiplication (BLAS)
val result = A :* B          // Element-wise (in-place capable)
val result = A.t             // Transpose (view, no copy)
```

### Numerical Stability

Special care for:
- **Log-space computations** to avoid underflow
- **Cholesky decomposition** for positive-definite matrices
- **SVD** instead of matrix inversion when possible
- **Welford's algorithm** for online mean/variance

Example:
```scala
// Avoid: direct computation (can overflow)
val likelihood = products.product

// Better: log-space
val logLikelihood = products.map(math.log).sum
val likelihood = math.exp(logLikelihood)
```

## Testing Strategy

### Unit Tests

```scala
class NormalTest extends FlatSpec with Matchers {
  "Normal distribution" should "have correct mean" in {
    val normal = Normal(5.0, 2.0)
    normal.mean() should be (5.0 +- 0.001)
  }
}
```

### Property-Based Tests

```scala
"All distributions" should "have PDF that integrates to 1" in {
  forAll { (mu: Double, sigma: Double) =>
    whenever(sigma > 0) {
      val normal = Normal(mu, sigma)
      val integral = integrate(normal.pdf, -10, 10)
      integral should be (1.0 +- 0.01)
    }
  }
}
```

### Example Tests

Examples in `examples/` serve as integration tests.

## Extension Points

### Adding New Distributions

1. Extend `ContinuousDistribution` or `DiscreteDistribution`
2. Implement required methods: `pdf`, `cdf`, `invcdf`, `draw`
3. Add tests
4. Document with ScalaDoc

### Adding New Regression Methods

1. Follow pattern of existing regression classes
2. Return immutable model
3. Provide `predict` and `op` methods
4. Include diagnostic statistics

### Adding New Layers (Neural Networks)

1. Extend `Layer` trait
2. Implement `forward` and `backward`
3. Handle weight initialization
4. Add to builder DSL

## Future Improvements

See [TODOs.md](TODOs.md) for planned work.

### Potential Enhancements

1. **Spark integration** - Distributed computing
2. **GPU acceleration** - CUDA support via Breeze
3. **Automatic differentiation** - For more flexible optimization
4. **More distributions** - Additional probability distributions
5. **Bayesian inference** - MCMC improvements
6. **Time series** - ARIMA, state space models
7. **Deep learning** - CNNs, RNNs, attention mechanisms

## Design Philosophy

### Principles

1. **Clarity over cleverness** - Readable code
2. **Functional when possible** - Immutability, pure functions
3. **Pragmatic OOP** - Use classes when appropriate
4. **Type safety** - Leverage Scala's type system
5. **Documentation** - Explain the why, not just the what
6. **Examples** - Show how to use features
7. **Testing** - Verify correctness

### Trade-offs

- **Clarity vs Performance**: Favor clarity (educational focus)
- **Flexibility vs Simplicity**: Keep APIs simple
- **Features vs Maintenance**: Limited scope for single maintainer
- **Accuracy vs Speed**: Reasonable accuracy without optimization

## Contributing

See [Contributing.md](Contributing.md) for guidelines on extending the architecture.

## References

### Inspirations

- **Breeze** - Scala numeric library design
- **NumPy/SciPy** - Python scientific computing
- **R** - Statistical computing language
- **Bishop's PRML** - Machine learning algorithms
- **Hastie et al. ESL** - Statistical learning methods

---

[← Back to Home](Home.md)

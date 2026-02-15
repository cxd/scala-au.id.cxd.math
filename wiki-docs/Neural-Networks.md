# Neural Networks

Build and train neural networks for classification and regression tasks.

## Overview

The library provides a flexible neural network framework with:
- **Customizable architectures** - Define layers and connections
- **Multiple activation functions** - Sigmoid, ReLU, Tanh, Softmax, etc.
- **Various loss functions** - MSE, cross-entropy, etc.
- **Training algorithms** - SGD, momentum, adaptive learning rates
- **Builder pattern** - Easy network construction

**Package**: `au.id.cxd.math.model.network`

## Basic Concepts

### Network Architecture

A neural network consists of:
1. **Input layer** - Receives input features
2. **Hidden layers** - Transform features (can be multiple)
3. **Output layer** - Produces predictions

### Key Components

- **Layers**: Transform inputs to outputs
- **Activations**: Non-linear functions applied after layers
- **Weights**: Learnable parameters
- **Loss function**: Measures prediction error
- **Optimizer**: Updates weights to minimize loss

## Building Networks

### Using the Builder Pattern

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._

// Create a simple network: 2 inputs -> 4 hidden -> 1 output
val network = Builder()
  .addLayer(Linear(inputSize = 2, outputSize = 4))
  .addLayer(Activation(Sigmoid))
  .addLayer(Linear(inputSize = 4, outputSize = 1))
  .addLayer(Activation(Sigmoid))
  .build()
```

### Network Types

#### Feedforward Neural Network

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._

// Binary classification network
val binaryClassifier = Builder()
  .addLayer(Linear(inputSize = 10, outputSize = 20))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(inputSize = 20, outputSize = 10))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(inputSize = 10, outputSize = 1))
  .addLayer(Activation(Sigmoid))  // Output probabilities
  .build()
```

#### Multi-class Classification Network

```scala
val multiClassifier = Builder()
  .addLayer(Linear(inputSize = 10, outputSize = 20))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(inputSize = 20, outputSize = 3))  // 3 classes
  .addLayer(Activation(Softmax))  // Probabilities sum to 1
  .build()
```

#### Regression Network

```scala
val regressor = Builder()
  .addLayer(Linear(inputSize = 5, outputSize = 10))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(inputSize = 10, outputSize = 5))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(inputSize = 5, outputSize = 1))
  // No activation for regression output
  .build()
```

## Activation Functions

### Available Activations

```scala
import au.id.cxd.math.model.network.activation._

// Common activation functions
Sigmoid      // σ(x) = 1 / (1 + e^(-x))
Tanh         // tanh(x)
ReLU         // max(0, x)
LeakyReLU    // max(αx, x) where α is small
ELU          // Exponential Linear Unit
Softmax      // e^(xi) / Σ(e^(xj))
Softplus     // log(1 + e^x)
Identity     // f(x) = x (for regression output)
```

### When to Use Each

| Activation | Use Case | Pros | Cons |
|------------|----------|------|------|
| **ReLU** | Hidden layers (default) | Fast, avoids vanishing gradient | Can "die" (always output 0) |
| **LeakyReLU** | Hidden layers | Addresses dying ReLU | Small gradient for negative inputs |
| **Tanh** | Hidden layers | Zero-centered | Can saturate |
| **Sigmoid** | Binary classification output | Outputs [0,1] | Vanishing gradient problem |
| **Softmax** | Multi-class output | Probability distribution | Only for output layer |
| **Identity** | Regression output | Direct output | No non-linearity |

### Example Usage

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._

val network = Builder()
  // First hidden layer with ReLU
  .addLayer(Linear(10, 20))
  .addLayer(Activation(ReLU))
  
  // Second hidden layer with LeakyReLU
  .addLayer(Linear(20, 10))
  .addLayer(Activation(LeakyReLU))
  
  // Output layer with Sigmoid
  .addLayer(Linear(10, 1))
  .addLayer(Activation(Sigmoid))
  .build()
```

## Training Networks

### Stochastic Gradient Descent (SGD)

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._
import au.id.cxd.math.model.network.loss.SquareError
import au.id.cxd.math.model.network.train.SGDTrainer
import breeze.linalg._

// Build network
val network = Builder()
  .addLayer(Linear(2, 4))
  .addLayer(Activation(Sigmoid))
  .addLayer(Linear(4, 1))
  .addLayer(Activation(Sigmoid))
  .build()

// Training data (XOR problem)
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
)

// Create trainer
val trainer = SGDTrainer(
  learningRate = 0.5,
  epochs = 1000,
  lossFunction = SquareError,
  batchSize = 4,  // Full batch
  verbose = true  // Print progress
)

// Train the network
val trainedNetwork = trainer.train(network, X, y)

// Make predictions
val predictions = trainedNetwork.forward(X)
println(s"Predictions:\n$predictions")
```

### Training Parameters

#### Learning Rate

```scala
// Too low: slow convergence
val slowTrainer = SGDTrainer(learningRate = 0.001, epochs = 10000, ...)

// Too high: unstable, might diverge
val unstableTrainer = SGDTrainer(learningRate = 10.0, epochs = 100, ...)

// Good starting point
val goodTrainer = SGDTrainer(learningRate = 0.01, epochs = 1000, ...)
```

#### Batch Size

```scala
// Full batch gradient descent
val fullBatch = SGDTrainer(batchSize = X.rows, ...)

// Mini-batch (recommended)
val miniBatch = SGDTrainer(batchSize = 32, ...)

// Stochastic (batch size = 1)
val stochastic = SGDTrainer(batchSize = 1, ...)
```

#### Momentum

```scala
// Add momentum to accelerate training
val momentumTrainer = SGDTrainer(
  learningRate = 0.01,
  epochs = 1000,
  momentum = 0.9,  // Typical value: 0.9
  lossFunction = SquareError
)
```

### Loss Functions

```scala
import au.id.cxd.math.model.network.loss._

// For regression
SquareError        // Mean Squared Error
AbsoluteError      // Mean Absolute Error

// For binary classification
CrossEntropy       // Binary cross-entropy

// For multi-class classification
CategoricalCrossEntropy  // Multi-class cross-entropy
```

### Early Stopping

```scala
var bestLoss = Double.MaxValue
var patience = 0
val maxPatience = 10
var bestWeights = network.getWeights()

for (epoch <- 0 until maxEpochs) {
  val trainedNet = trainer.trainOneEpoch(network, X, y)
  val loss = computeLoss(trainedNet, X_val, y_val)
  
  if (loss < bestLoss) {
    bestLoss = loss
    bestWeights = trainedNet.getWeights()
    patience = 0
  } else {
    patience += 1
    if (patience >= maxPatience) {
      println(s"Early stopping at epoch $epoch")
      break
    }
  }
}

// Restore best weights
network.setWeights(bestWeights)
```

## Making Predictions

### Forward Pass

```scala
// Single prediction
val input = DenseMatrix((0.5, 0.7))
val output = network.forward(input)
println(s"Prediction: ${output(0, 0)}")

// Batch predictions
val inputs = DenseMatrix(
  (0.1, 0.2),
  (0.3, 0.4),
  (0.5, 0.6)
)
val outputs = network.forward(inputs)
println(s"Batch predictions:\n$outputs")
```

### Classification

```scala
// Binary classification
val prob = network.forward(input)(0, 0)
val prediction = if (prob > 0.5) 1 else 0
println(s"Class: $prediction (probability: $prob)")

// Multi-class classification
val probs = network.forward(input)
val predictedClass = argmax(probs(0, ::).t)
println(s"Predicted class: $predictedClass")
```

## Model Evaluation

### Accuracy

```scala
def accuracy(predictions: DenseMatrix[Double], 
             labels: DenseMatrix[Double]): Double = {
  val predicted = predictions.map(p => if (p > 0.5) 1.0 else 0.0)
  val correct = (predicted :== labels).activeSize
  correct.toDouble / labels.rows
}

val acc = accuracy(predictions, y)
println(f"Accuracy: ${acc * 100}%.2f%%")
```

### Confusion Matrix

```scala
def confusionMatrix(predictions: DenseMatrix[Double],
                   labels: DenseMatrix[Double]): DenseMatrix[Int] = {
  val pred = predictions.map(p => if (p > 0.5) 1 else 0)
  val true_labels = labels.map(_.toInt)
  
  var tp = 0  // True positives
  var fp = 0  // False positives
  var tn = 0  // True negatives
  var fn = 0  // False negatives
  
  for (i <- 0 until pred.rows) {
    (pred(i, 0), true_labels(i, 0)) match {
      case (1, 1) => tp += 1
      case (1, 0) => fp += 1
      case (0, 0) => tn += 1
      case (0, 1) => fn += 1
    }
  }
  
  DenseMatrix((tp, fp), (fn, tn))
}

val cm = confusionMatrix(predictions, y)
println(s"Confusion Matrix:\n$cm")

// Derived metrics
val precision = cm(0, 0).toDouble / (cm(0, 0) + cm(0, 1))
val recall = cm(0, 0).toDouble / (cm(0, 0) + cm(1, 0))
val f1 = 2 * (precision * recall) / (precision + recall)

println(f"Precision: ${precision * 100}%.2f%%")
println(f"Recall: ${recall * 100}%.2f%%")
println(f"F1-score: ${f1 * 100}%.2f%%")
```

## Advanced Techniques

### Regularization

#### L2 Regularization (Weight Decay)

```scala
val trainer = SGDTrainer(
  learningRate = 0.01,
  epochs = 1000,
  lossFunction = SquareError,
  l2Lambda = 0.001  // L2 regularization coefficient
)
```

#### Dropout

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._
import au.id.cxd.math.model.network.regularization._

val network = Builder()
  .addLayer(Linear(10, 20))
  .addLayer(Activation(ReLU))
  .addLayer(Dropout(0.5))  // Drop 50% of neurons during training
  .addLayer(Linear(20, 10))
  .addLayer(Activation(ReLU))
  .addLayer(Dropout(0.3))  // Drop 30% of neurons
  .addLayer(Linear(10, 1))
  .build()
```

### Batch Normalization

```scala
val network = Builder()
  .addLayer(Linear(10, 20))
  .addLayer(BatchNorm(20))  // Normalize activations
  .addLayer(Activation(ReLU))
  .addLayer(Linear(20, 10))
  .addLayer(BatchNorm(10))
  .addLayer(Activation(ReLU))
  .addLayer(Linear(10, 1))
  .build()
```

### Learning Rate Schedules

```scala
// Step decay
def stepDecay(initialLR: Double, epoch: Int, dropEvery: Int = 10): Double = {
  initialLR * math.pow(0.5, math.floor(epoch / dropEvery))
}

// Exponential decay
def expDecay(initialLR: Double, epoch: Int, decayRate: Double = 0.95): Double = {
  initialLR * math.pow(decayRate, epoch)
}

// Cosine annealing
def cosineAnneal(initialLR: Double, epoch: Int, totalEpochs: Int): Double = {
  initialLR * 0.5 * (1 + math.cos(math.Pi * epoch / totalEpochs))
}

// Use in training loop
for (epoch <- 0 until maxEpochs) {
  val currentLR = stepDecay(initialLR = 0.1, epoch)
  val trainer = SGDTrainer(learningRate = currentLR, ...)
  network = trainer.trainOneEpoch(network, X, y)
}
```

## Complete Example

### XOR Problem

```scala
import au.id.cxd.math.model.network.builder._
import au.id.cxd.math.model.network.activation._
import au.id.cxd.math.model.network.loss.SquareError
import au.id.cxd.math.model.network.train.SGDTrainer
import breeze.linalg._

// XOR is not linearly separable - needs hidden layer
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
)

// Build network
val network = Builder()
  .addLayer(Linear(2, 4))
  .addLayer(Activation(Sigmoid))
  .addLayer(Linear(4, 1))
  .addLayer(Activation(Sigmoid))
  .build()

// Train
val trainer = SGDTrainer(
  learningRate = 0.5,
  epochs = 5000,
  lossFunction = SquareError,
  verbose = true
)

val trained = trainer.train(network, X, y)

// Evaluate
val predictions = trained.forward(X)
println("Predictions:")
for (i <- 0 until X.rows) {
  val input = X(i, ::).t
  val expected = y(i, 0)
  val predicted = predictions(i, 0)
  println(f"Input: $input, Expected: $expected%.0f, Predicted: $predicted%.4f")
}
```

## Best Practices

1. **Start simple** - Begin with small networks
2. **Normalize inputs** - Scale features to similar ranges
3. **Use ReLU** - Default activation for hidden layers
4. **Monitor training** - Plot loss curves
5. **Use validation set** - Detect overfitting early
6. **Experiment with architecture** - Try different layer sizes
7. **Tune hyperparameters** - Learning rate, batch size, etc.
8. **Use regularization** - Prevent overfitting
9. **Initialize weights properly** - Xavier/He initialization
10. **Save best model** - Keep weights with lowest validation loss

## Common Issues

| Problem | Solution |
|---------|----------|
| **Training loss not decreasing** | Reduce learning rate, check data, verify implementation |
| **Loss is NaN** | Lower learning rate, check for numerical instability |
| **Overfitting** | Add regularization (L2, dropout), reduce model size |
| **Underfitting** | Increase model capacity, train longer, reduce regularization |
| **Slow convergence** | Increase learning rate, use momentum, batch normalization |
| **Predictions all same** | Check loss function, verify data labels, adjust architecture |

## See Also

- [Regression Methods](Regression-Methods.md) - Linear regression alternatives
- [Examples Catalog](Examples-Catalog.md) - Neural network examples
- [Data Processing](Data-Processing.md) - Preparing data for training
- [API Quick Reference](API-Quick-Reference.md) - Quick API lookup

---

[← Back to Home](Home.md)

package au.id.cxd.math.model.network.train

import au.id.cxd.math.function.matrix.LogMult
import au.id.cxd.math.model.network.builder.Sequence
import au.id.cxd.math.model.network.layers.Layer
import au.id.cxd.math.model.network.loss.{Loss, MeanSquareErrorLoss}
import breeze.linalg.{DenseMatrix, tile}

import scala.annotation.tailrec
import scala.collection.mutable

class SGDTrainer(override val trainX: DenseMatrix[Double],
                 override val trainY: DenseMatrix[Double],
                 override val validX: DenseMatrix[Double],
                 override val validY: DenseMatrix[Double],
                 override val lossFn: Loss = MeanSquareErrorLoss(),
                 batchSize: Int = 32,
                 learnRate: Double = 0.02,
                 momentum: Double = 0.0,
                 lossThreshold: Double = Math.E,
                 verbose:Boolean = true,
                 bias:Double = 1.0) extends Trainer {


  /**
    * add bias to the training data.
    * @param data
    * @return
    */
  def addBias(data:DenseMatrix[Double], network:Sequence):DenseMatrix[Double] = {
    network.addBias(data, bias)
  }

  /**
    * compute the gradient for the output layer
    *
    * The gradient is a vector that is the same size as the number of units in the layer.
    *
    * @param errors
    * @param network
    * @return
    */
  def outputGradient(errors: DenseMatrix[Double], network: Sequence): DenseMatrix[Double] = {

    // compute the local gradient of output node with respect to weights and errors.
    val d_j = network.layers.last.derivative
    val y_i = network.layers.reverse.tail.head.output

    /**
      * compute the gradient
      * At the output layer the gradient is the element wise multiplication between
      * derivative and error.
      *
      * Note in the case of softmax derivative we want the dot product of the errors and the derivative.
      * Since the derivative is a martrix of units x units size.
      * Whereas if the other activation functions are used we just want the memberwise product *:*.
      */
    val grad = if (errors.cols == d_j.rows && d_j.cols > 1) (errors * d_j).t
               else errors.t *:* d_j

    grad

  }

  /**
    * compute the gradient for the hidden layers.
    *
    * The local gradient is a vector that is the same length as the number of units in the layer.
    *
    * @param gradient
    * @param network
    * @return
    */
  def hiddenGradient(gradient: Seq[DenseMatrix[Double]], network: Sequence): Seq[DenseMatrix[Double]] = {

    @tailrec
    def gradients(accum: Seq[DenseMatrix[Double]], lastLayer:Layer, layers: Seq[Layer]): Seq[DenseMatrix[Double]] = {
      layers.size == 1 match {
        case true => accum
        case _ => {
          val hidden = layers.head
          val grad_k = accum.last

          // grad_l = W_{l+1}^T grad_{l+1} *:* sigma_l

          // we have a gradient vector input, we multiply
          // \sigma_k w_{kj}
          // we expect the gradient to have same number of columns as units
          // so we take the columnwise sums of the gradient

          val g_k = if (lastLayer.weights.cols != grad_k.rows) grad_k.t
                    else grad_k

          val a = lastLayer.weights * g_k

          val g = hidden.derivative *:* a
          gradients(accum :+ g.t, hidden, layers.tail)
        }
      }
    }

    // we are working from the hidden layers backward
    val hiddenLayers = network.layers.reverse.tail
    gradients(gradient, network.layers.last, hiddenLayers).reverse

  }

  /**
    * compute the gradients going backwards from output to lowest hidden layer.
    *
    * @param errors
    * @param network
    * @return
    */
  def computeGradients(errors: DenseMatrix[Double], network: Sequence): Seq[DenseMatrix[Double]] = {
    hiddenGradient(Seq(outputGradient(errors, network)), network)
  }

  /**
    * compute weight updates for each hidden layer and the output layer having the gradients.
    *
    * @param gradient
    * @param network
    * @return
    */
  def computeDeltaWeights(gradient: Seq[DenseMatrix[Double]], network: Sequence): Seq[DenseMatrix[Double]] = {
    // we compute weights for hidden layers upwards.
    @tailrec
    def weightDeltas(accum: Seq[DenseMatrix[Double]], prevLayer: Layer, layerAndGrad: Seq[(Layer, DenseMatrix[Double])]): Seq[DenseMatrix[Double]] = {
      layerAndGrad.size > 0 match {
        case false => accum
        case true => {
          val h = prevLayer.output
          val (layer, grad) = layerAndGrad.head
          val (wrows, wcols) = layer.shape.get

          val m = layer.priorWeights match {
            case None => DenseMatrix.zeros[Double](wrows, wcols)
            case Some(w) => momentum * w
          }

          val step = if (h.cols != grad.rows) learnRate * h * grad.t
                     else learnRate * h * grad


          val delta = m + step
          weightDeltas(accum :+ delta, layer, layerAndGrad.tail)
        }
      }
    }

    val input = network.layers.head
    val hiddenAndGrad = network.layers.tail.zip(gradient)
    weightDeltas(Seq[DenseMatrix[Double]](), input, hiddenAndGrad)
  }

  /**
    * apply the changes to the hidden layer
    *
    * @param deltas
    * @param network
    * @return
    */
  def applyWeightChanges(deltas: Seq[DenseMatrix[Double]], network: Sequence): Sequence = {
    val input = network.layers.head
    val hidden = network.layers.tail
    val updatedLayers = (hidden.zip(deltas)).map {
      pair => {
        val (layer, delta) = pair
        val prevWeights = layer.weights
        val newWeights = layer.weights + delta
        layer.withWeights(newWeights)
          .withPriorWeights(prevWeights)
      }
    }
    Sequence(Seq(input) ++ updatedLayers, network.weightInitialisation)
  }

  /**
    * train the network for N epochs
    *
    * this method is by and large imperative
    *
    * return the validation loss and training loss.
    *
    * @param epochs
    */
  override def train(epochs: Int)(implicit network: Sequence): (Seq[Double], Seq[Double], Sequence) = {

    var trainNet: Sequence = network

    val trainloss = mutable.Buffer[Double]()
    val validationloss = mutable.Buffer[Double]()

    var exitLoss = Double.MaxValue

    val range = (for (i <- 1 to epochs) yield i)
      .takeWhile(_ => exitLoss > lossThreshold)


    val trainDataX = addBias(trainX, network)
    val validDataX = addBias(validX, network)

    for (epoch <- range) {

      var alloutputs = DenseMatrix.zeros[Double](0,0)
      var alltargets = DenseMatrix.zeros[Double](0,0)

      var valoutputs = DenseMatrix.zeros[Double](0,0)
      var valtargets = DenseMatrix.zeros[Double](0,0)

      // we process each of the rows of the training input.
      for (i <- 0 until trainDataX.rows) {

        val input = trainDataX(i, ::).inner.toDenseMatrix
        val target = trainY(i, ::).inner.toDenseMatrix

        val output = trainNet.transfer(input)

        if (alloutputs.cols == 0) {
          alloutputs = output.t
        } else {
          alloutputs = DenseMatrix.vertcat(alloutputs, output.t)
        }
        if (alltargets.cols == 0) {
          alltargets = target
        } else {
          alltargets = DenseMatrix.vertcat(alltargets, target)
        }

        // we have a loss function we need to calculate the error signal
        // and we then need to calculate the derivative of the error signal
        // in order to propagate the gradient backward
        // and update the weights.

        val (loss, errors) = lossFn(target, output.t)

        // calculate the gradients
        val grads = computeGradients(errors, trainNet)

        // now we propogate the gradients calculate the changes to weights
        val deltas = computeDeltaWeights(grads, trainNet)

        trainNet = applyWeightChanges(deltas, trainNet)


      }
      // calculate loss for epoch


      val (loss, errors) = lossFn(alltargets, alloutputs)

      for (i <- 0 until validDataX.rows) {
        val input = validDataX(i, ::).inner.toDenseMatrix
        val target = validY(i, ::).inner.toDenseMatrix
        val output = trainNet.transfer(input)

        if (valtargets.cols == 0) {
          valtargets = target
        } else {
          valtargets = DenseMatrix.vertcat(valtargets, target)
        }
        if (valoutputs.cols == 0) {
          valoutputs = output.t
        } else {
          valoutputs = DenseMatrix.vertcat(valoutputs, output.t)
        }
      }

      // calculate the validation loss and accuracy
      val (valloss, valerrors) = lossFn(valtargets, valoutputs)

      trainloss += loss
      validationloss += valloss

      if (verbose) {
        println(s"Epoch $epoch Train Loss $loss Validation Loss: $valloss")
      }
      // we use the validation loss to determine the exit threshold.
      exitLoss = valloss
    }

    (trainloss.toSeq, validationloss.toSeq, trainNet)

  }


}

object SGDTrainer {
  def apply(trainX: DenseMatrix[Double],
            trainY: DenseMatrix[Double],
            validX: DenseMatrix[Double],
            validY: DenseMatrix[Double],
            lossFn: Loss = MeanSquareErrorLoss(),
            batchSize: Int = 32,
            learnRate: Double = 0.02,
            momentum: Double = 0.0,
            lossThreshold: Double = Math.E,
            verbose:Boolean = true) = new SGDTrainer(trainX,
    trainY,
    validX,
    validY,
    lossFn,
    batchSize,
    learnRate,
    momentum,
    lossThreshold,
    verbose)
}
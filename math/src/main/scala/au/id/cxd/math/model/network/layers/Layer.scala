package au.id.cxd.math.model.network.layers

import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.network.activation.Activation
import au.id.cxd.math.model.network.initialisation.WeightInitialisationStrategy
import breeze.linalg.DenseMatrix

trait Layer extends Serializable {

  /**
    * a flag indicating whether to use batch normalisation
    * on the layers activation.
    */
  val batchNormalise:Boolean = false
  /**
    * activation function
    */
  val activation:Activation

  /**
    * weight initialisation strategy
    */
  val weightInitialisation:WeightInitialisationStrategy = new WeightInitialisationStrategy {}

  /**
    * number of units at the layer
    */
  val units:Int

  /**
    * shape of layer.
    */
  val shape:Option[(Int,Int)] = None

  /**
    * weight matrix
    */
  var weights:DenseMatrix[Double] = DenseMatrix.ones[Double](1,1)

  /**
    * previous weights can optionally be used during training
    * such as in stochastic gradient descent with momentum.
    */
  var priorWeights:Option[DenseMatrix[Double]] = None

  /**
    * derivative of activation function
    */
  var derivative:DenseMatrix[Double] = DenseMatrix.ones[Double](1,1)

  /**
    * output of the transfer function
    */
  var output:DenseMatrix[Double] = DenseMatrix.ones[Double](1,1)


  /**
    * copy with shape parameter
    * @param newShape
    * @return
    */
  def withShape(newShape:Option[(Int,Int)]):Layer

  /**
    * initialise with weights
    * @param newWeights
    * @return
    */
  def withWeights(newWeights:DenseMatrix[Double]):Layer

  /**
    * allocate prior weights
    * @param priorWeights
    * @return
    */
  def withPriorWeights(priorWeights:DenseMatrix[Double]):Layer

  /**
    * update the units in the layer
    * @param units
    * @return
    */
  def withUnits(units:Int):Layer

  /**
    * batch normalise the supplied parameter.
    * @param m
    * @return
    */
  def runbatchNormalise(m:DenseMatrix[Double]):DenseMatrix[Double] = {
    StandardisedNormalisation().transform(m)
  }

  /**
    * perform the transfer function
    * @param x
    * @return
    */
  def transfer(x:DenseMatrix[Double]):DenseMatrix[Double] = {

    val x1 = batchNormalise match {
      case false => x
      case _ => runbatchNormalise(x)
    }

    val h =  weights.t * x1
    output = activation(h)
    // if batch normalisation is enabled apply batch normalisation.



    // the local derivative given the output of the previous layer.
    // to compute the full derivative at the layer we still need to apply the chain rule.
    // L_n' = L_n' L_{n-1}' L_{n-2}' \cdots L_0'

    derivative = activation.derivative(h)

    output
  }

}

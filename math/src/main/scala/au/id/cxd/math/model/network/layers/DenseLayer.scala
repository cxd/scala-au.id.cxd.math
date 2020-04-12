package au.id.cxd.math.model.network.layers

import au.id.cxd.math.model.network.activation.Activation
import au.id.cxd.math.model.network.initialisation.WeightInitialisationStrategy
import breeze.linalg.DenseMatrix

/**
  * construct a dense network layer
  *
  *
  * The activation layer is mandatory,
  *
  * each layer must have units specified.
  *
  * @param activation
  * @param shape
  */
class DenseLayer(override val activation: Activation,
                 override val units:Int,
                 override val shape: Option[(Int, Int)] = None,
                 override val batchNormalise: Boolean = false,
                 override val weightInitialisation: WeightInitialisationStrategy = new WeightInitialisationStrategy {}) extends Layer {

  /**
    * default weights
    */
  weights = shape match {
    case None => weights
    case Some((rows, cols)) => initialiseWeights(rows, cols)
  }

  /**
    * convenience method used to initialise weights at a later time.
    * @param rows
    * @param cols
    * @return
    */
  def initialiseWeights(rows:Int, cols:Int): DenseMatrix[Double] = weightInitialisation.op(rows, cols)

  /**
    * copy with shape parameter
    *
    * @param newShape
    * @return
    */
  override def withShape(newShape: Option[(Int, Int)]): Layer = {
    val dense = new DenseLayer(activation, units, newShape)
    dense.weights = weights
    dense.output = output
    dense.derivative = derivative
    dense
  }

  /**
    * initialise with weights
    *
    * @param newWeights
    * @return
    */
  override def withWeights(newWeights: DenseMatrix[Double]): Layer = {
    val dense = new DenseLayer(activation, units, shape)
    dense.weights = newWeights
    dense.output = output
    dense.derivative = derivative
    dense
  }

  /**
    * allocate prior weights
    * @param priorWeights
    * @return
    */
  override def withPriorWeights(priorWeights:DenseMatrix[Double]):Layer = {
    val dense = new DenseLayer(activation, units, shape)
    dense.weights = weights
    dense.output = output
    dense.derivative = derivative
    dense.priorWeights = Some(priorWeights)
    dense
  }

  /**
    * update the units in the layer
    *
    * @param newUnits
    * @return
    */
  override def withUnits(newUnits: Int): Layer = {
    val dense = new DenseLayer(activation, units = newUnits, shape=this.shape)
    dense.weights = weights
    dense.output = output
    dense.derivative = derivative
    dense.priorWeights = priorWeights
    dense
  }
}

object DenseLayer {
  def apply(activation: Activation, units:Int, shape: Option[(Int, Int)] = None,
            weightInitialisation:WeightInitialisationStrategy=new WeightInitialisationStrategy {}):DenseLayer = {
    new DenseLayer(activation, units, shape, weightInitialisation=weightInitialisation)
  }
}
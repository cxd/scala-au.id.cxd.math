package au.id.cxd.math.model.network.layers

import au.id.cxd.math.model.network.activation.Activation
import breeze.linalg.DenseMatrix

class InputLayer(override val activation: Activation, override val units:Int, override val shape: Option[(Int, Int)] = None)
  extends DenseLayer(activation, units, shape) {

  /**
    * the input transfer function does not multiply by weights
    * The outut of the input layer is the transpose of the input.
    * (rows x cols) becomes (cols x rows)
    * @param x
    * @return
    */
  override def transfer(x: DenseMatrix[Double]): DenseMatrix[Double] = {
    output = x.t
    output
  }

  /**
    * copy with shape parameter
    *
    * @param newShape
    * @return
    */
  override def withShape(newShape: Option[(Int, Int)]): Layer = {
    val dense = new InputLayer(activation, units, newShape)
    dense.weights = weights
    dense.output = dense.output
    dense.derivative = dense.derivative
    dense
  }

  /**
    * initialise with weights
    *
    * @param newWeights
    * @return
    */
  override def withWeights(newWeights: DenseMatrix[Double]): Layer = {
    val dense = new InputLayer(activation, units, shape)
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
    val dense = new InputLayer(activation, units, shape)
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
    val dense = new InputLayer(activation, units = newUnits, shape=this.shape)
    dense.weights = weights
    dense.output = output
    dense.derivative = derivative
    dense.priorWeights = priorWeights
    dense
  }
}
object InputLayer {
  def apply(activation: Activation, units:Int, shape: Option[(Int, Int)] = None):InputLayer = {
    new InputLayer(activation, units, shape)
  }
}

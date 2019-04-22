package au.id.cxd.math.model.network.builder

import au.id.cxd.math.model.network.initialisation.WeightInitialisationStrategy
import au.id.cxd.math.model.network.layers.Layer
import breeze.linalg.DenseMatrix

trait Builder extends Serializable {

  /**
    * sequence of layers.
    */
  val layers:Seq[Layer]

  /**
    * a weight initialisation strategy
    */
  val weightInitialisation:WeightInitialisationStrategy

  /**
    * having defined a sequence of nodes starting with an input node
    * terminating with an output node.
    * Initialise all intermediate nodes.
    * Note the output size is expected to be specified.
    *
    * The weights are not initialised until training,
    * however the dimensions of the weights at each layer are defined.
    * The dimension being:
    *
    * (Units_n, Units_{n-1})
    *
    * During training only the X input is transposed initially.
    * All other matrix operations can be performed by transposing weights
    * and using the outputs from the last layer.
    * @return
    */
  def compile():Builder

  def initialiseWeights():Builder

  /**
    * propogate data forward through the network.
    * @param x
    * @return
    */
  def transfer(x:DenseMatrix[Double]):DenseMatrix[Double] = {

    val output = layers.foldLeft(x) {
      (h, layer) => {
        val o = layer.transfer(h)
        o
      }
    }
    output
  }

}

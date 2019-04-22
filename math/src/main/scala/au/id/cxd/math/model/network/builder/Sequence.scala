package au.id.cxd.math.model.network.builder

import au.id.cxd.math.model.network.initialisation.{RandomWeightInitialisation, WeightInitialisationStrategy}
import au.id.cxd.math.model.network.layers.{Layer,InputLayer}

/**
  * generate a sequence of nodes
  * @param layers
  */
class Sequence(override val layers:Seq[Layer],
               override val weightInitialisation: WeightInitialisationStrategy = RandomWeightInitialisation()) extends Builder {

  val input:Layer = layers.head

  val output:Layer = layers.last

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
  override def compile():Builder = {

    // number of input columns are mandatory.

    // inputs + 1 column for bias
    val newLayers = layers.map {
      l => if (l.isInstanceOf[InputLayer]) {
        l.withUnits(l.units + 1)
      } else l
    }

    val inputs :Int = layers.head.units

    val (startRow, startCol) = (1, inputs)
    val pairs = newLayers.foldLeft ((Seq[Layer](), startCol)) {
      (pairs, layer) =>
        val (accum, prevUnits) = pairs
        val accum1 = accum :+ layer.withShape(Some(prevUnits, layer.units))
        (accum1, layer.units)
    }

    new Sequence(pairs._1, weightInitialisation)
      .initialiseWeights()
  }

  /**
    * initialise weights in the layers
    * @return
    */
  override def initialiseWeights(): Builder = {
    val newLayers = layers.map {
      layer =>
        val (rows,cols) = layer.shape.get
        if (layer.isInstanceOf[InputLayer]) layer
        else layer.withWeights(weightInitialisation.op(rows, cols))
    }
    new Sequence(newLayers, weightInitialisation)
  }
}
object Sequence {
  def apply(layers:Seq[Layer],
  weightInitialisation: WeightInitialisationStrategy = RandomWeightInitialisation()) =
    new Sequence(layers, weightInitialisation)
}

package au.id.cxd.math.model.network.builder

import au.id.cxd.math.data.filter.Which
import au.id.cxd.math.model.network.initialisation.WeightInitialisationStrategy
import au.id.cxd.math.model.network.layers.Layer
import breeze.linalg.DenseMatrix

trait Builder extends Serializable {

  /**
    * sequence of layers.
    */
  val layers:Seq[Layer]

  val bias:Double = 1.0

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
    * add bias to the training data.
    * @param data
    * @return
    */
  def addBias(data:DenseMatrix[Double], biasVal:Double = bias):DenseMatrix[Double] = {
    val b = bias*DenseMatrix.ones[Double](data.rows, 1)
    DenseMatrix.horzcat(data, b)
  }

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

  /**
    * predict continuous variables.
    * @param x
    * @return
    */
  def predict(x:DenseMatrix[Double]):DenseMatrix[Double] = {
    val input = addBias(x)
    transfer(x)
  }

  /**
    * perform the classification task
    * given the data set identify the maximum class label associated with
    * the output of each record in the network.
    * @param x
    * @param labels
    * @return
    */
  def classify(x:DenseMatrix[Double], labels:List[String]):Seq[String] = {
    val input = addBias(x)
    val range = for (i <- 0 until x.rows) yield i
    range.foldLeft(Seq[String]()) {
      (accum, i) => {
        val row = input(i,::)
        val output = transfer(row.inner.toDenseMatrix)
        // we now need to find the maximum output value.
        val maxS = output.toArray.max
        val idx = Which[Double](output.toArray, d => d == maxS)
        val label = labels(idx.head)
        accum :+ label
      }
    }
  }

}

package au.id.cxd.math.model.network.train

import au.id.cxd.math.model.network.builder.Sequence
import au.id.cxd.math.model.network.loss.Loss
import breeze.linalg.DenseMatrix

trait Trainer {

  val trainX:DenseMatrix[Double]

  val trainY:DenseMatrix[Double]

  val validX:DenseMatrix[Double]

  val validY:DenseMatrix[Double]

  /**
    * loss function.
    */
  val lossFn:Loss

  /**
    * train the network for N epochs
    * @param epochs
    */
  def train(epochs:Int)(implicit network:Sequence):  (Seq[Double], Seq[Double], Sequence)
}

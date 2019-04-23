package au.id.cxd.math.model.network.loss

import breeze.linalg.DenseMatrix

trait Accuracy {

  /**
    * calculate accuracy
    * @param obs
    * @param sim
    */
  def accuracy(obs:DenseMatrix[Double], sim:DenseMatrix[Double])

}

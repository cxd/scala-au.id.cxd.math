package au.id.cxd.math.model.network.loss

import breeze.linalg.DenseMatrix

trait Loss {

  /**
    * calculate the loss given observed and simulated data.
    * @param obs
    * @param sim
    * @return
    */
  def apply(obs:DenseMatrix[Double], sim:DenseMatrix[Double]):(Double, DenseMatrix[Double])

}

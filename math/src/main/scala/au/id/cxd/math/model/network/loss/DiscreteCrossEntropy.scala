package au.id.cxd.math.model.network.loss

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * compute the cross entropy loss
  * for two distributions where
  * obs represents the distribution for the observed data
  * and sim represents the distribution for predicted data.
  */
case class DiscreteCrossEntropy() extends Loss {
  /**
    * calculate the loss given observed and simulated data.
    *
    * @param obs
    * @param sim
    * @return
    */
  override def apply(obs: DenseMatrix[Double], sim: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
    // errors are the derivative of the loss function.
    // the gradient here is positive the true derivative is negative.
    //
    val errors = DenseMatrix.tabulate(obs.rows, obs.cols) {
      case (i, j) =>
        obs(i, j) - sim(i, j)
    }
    // loss function.
    val loss = DenseMatrix.tabulate(obs.rows, obs.cols) {
      case (i, j) =>
        if (sim(i, j) != 0) obs(i, j) * Math.log(sim(i, j))
        else 0.0
    }.toArray.sum
    (-loss, errors)
  }

}

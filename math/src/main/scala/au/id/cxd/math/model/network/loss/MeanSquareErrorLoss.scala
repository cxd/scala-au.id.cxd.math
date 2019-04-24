package au.id.cxd.math.model.network.loss
import au.id.cxd.math.model.evaluation.{MAE, MSE}
import breeze.linalg.DenseMatrix

case class MeanSquareErrorLoss() extends Loss {
  /**
    * calculate the loss given observed and simulated data.
    *
    * @param obs
    * @param sim
    * @return
    */
  override def apply(obs: DenseMatrix[Double], sim: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
    val errors = if (obs.cols == sim.cols) obs - sim
                 else if (obs.cols > sim.cols) obs - sim.t
                 else obs.t - sim
    val mse = MSE(obs.toArray, sim.toArray)
    (mse, errors)
  }
}

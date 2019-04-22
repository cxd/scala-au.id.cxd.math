package au.id.cxd.math.model.network.initialisation

import au.id.cxd.math.probability.random.RNormal
import breeze.linalg.DenseMatrix

/**
  * weight initialisation from a random guassian distribution
  */
case class RandomGaussianInitialisation(val seed: Long = 42L, val mu: Double = 0.0, val sigma: Double = 1.0)
  extends WeightInitialisationStrategy {

  val rand = RNormal(mu = mu, sigma = sigma, seed = seed)

  /**
    * initialisation strategy.
    *
    * @param rows
    * @param cols
    * @return
    */
  override def op(rows: Int, cols: Int): DenseMatrix[Double] =
    DenseMatrix.tabulate(rows, cols) {
      case (i,j) => rand.draw()
    }
}


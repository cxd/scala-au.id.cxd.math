package au.id.cxd.math.model.network.loss
import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * compute the cross entropy loss
  * for two distributions where
  * obs represents the distribution for the observed data
  * and sim represents the distribution for predicted data.
  */
case class DiscreteCrossEntropy() extends Loss with Accuracy {
  /**
    * calculate the loss given observed and simulated data.
    *
    * @param obs
    * @param sim
    * @return
    */
  override def apply(obs: DenseMatrix[Double], sim: DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
    val errors = DenseMatrix.tabulate (obs.rows, obs.cols) {
      case (i, j) => obs(i,j) - sim(i,j)
    }
    val loss = DenseMatrix.tabulate(obs.rows, obs.cols) {
      case (i, j) =>
        if (sim(i,j) != 0) obs(i,j) * Math.log(sim(i, j))
        else 0.0
    }.toArray.sum
    (-loss, errors)
  }

  /**
    * calculate accuracy
    * @param obs
    * @param sim
    */
  override def accuracy(obs:DenseMatrix[Double], sim:DenseMatrix[Double]) = {

    // count up the matches
    val matches = DenseVector.tabulate(obs.rows) {
      case i => {
        val obsRow = obs(i,::).inner.toArray
        val simRow = sim(i,::).inner.toArray

        val (idx, maxVal) = simRow.foldLeft((-1, Double.MinValue)) {
          (accum, s) => if (s > accum._2) (accum._1+1, s)
          else (accum._1+1, accum._2)
        }
        val (idx1, maxVal1) = obsRow.foldLeft((-1, Double.MinValue)) {
          (accum, s) => if (s > accum._2) (accum._1+1, s)
          else (accum._1+1, accum._2)
        }

        if (idx1 == idx) 1.0
        else 0.0
      }
    }.toArray

    val matched = matches.sum
    val total = obs.rows

    matched / matches.sum

  }
}

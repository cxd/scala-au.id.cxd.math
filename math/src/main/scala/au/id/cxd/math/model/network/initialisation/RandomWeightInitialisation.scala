package au.id.cxd.math.model.network.initialisation
import au.id.cxd.math.probability.random.RUniform
import breeze.linalg.DenseMatrix

import scala.util.Random

/**
  * simple weight initialisation choosing a random double between 0 and 1.0
  *
  * Some suggested initialisations are:
  *
  * $$
  * W_{i,j} = U(-1/\sqrt{m}, 1/\sqrt{m})
  * $$
  *
  * Where $m$ is the number of columns in the input layer.
  *
  * $$
  * W_{i,j} = U(-\sqrt{6/(m+n), \sqrt{6/(m+n)})
  * $$
  *
  * Where $m$ is the number of columns in the input layer and $n$ the number of rows in the data.
  *
  * @param seed
  */
case class RandomWeightInitialisation(val seed:Long = 42L, val range:(Double,Double)=(-1.0,1.0)) extends WeightInitialisationStrategy {

  val rand = RUniform(min = range._1, max=range._2, seed=seed)


  /**
    * initialisation strategy.
    *
    * @param rows
    * @param cols
    * @return
    */
  override def op(rows: Int, cols: Int): DenseMatrix[Double] = DenseMatrix.tabulate(rows,cols) {
    case (i,j) => rand.draw
  }
}

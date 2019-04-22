package au.id.cxd.math.model.network.initialisation

import breeze.linalg.DenseMatrix

/**
  * weight initialisation strategies may differ depending on method
  * chosen.
  */
trait WeightInitialisationStrategy {

  /**
    * initialisation strategy.
    * @param rows
    * @param cols
    * @return
    */
  def op(rows:Int, cols:Int) :DenseMatrix[Double]

}

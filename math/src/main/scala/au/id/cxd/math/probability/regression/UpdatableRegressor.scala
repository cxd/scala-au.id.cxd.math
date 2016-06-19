package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * An updateable regressor can be updated after the initial training
  * Created by cd on 19/06/2016.
  */
trait UpdatableRegressor {
  /**
    * apply update to the trained model.
    * @param newX
    * @param newY
    */
  def update(newX: DenseMatrix[Double], newY: DenseVector[Double]) : (DenseMatrix[Double], Double)
}

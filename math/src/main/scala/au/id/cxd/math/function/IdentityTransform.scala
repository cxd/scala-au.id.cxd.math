package au.id.cxd.math.function

import breeze.linalg.DenseMatrix

/**
  * The identity transform will return the input unchanged.
  * Created by cd on 14/05/2016.
  */
class IdentityTransform extends ContinuousTransform {
  /**
    * transform the continuous matrix.
    *
    * @param continousData
    * @return
    */
  def transform(continousData: DenseMatrix[Double]): DenseMatrix[Double] = continousData

  /**
    * already having calculated to parameters simply
    * filter the supplied data through the transformation.
    * @param data
    * @return
    */
  def filter (data:DenseMatrix[Double]) = data
}
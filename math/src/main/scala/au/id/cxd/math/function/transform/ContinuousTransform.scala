package au.id.cxd.math.function.transform

import breeze.linalg.DenseMatrix

/**
  * A trait that describes a transformation for preprocessing continuous data.
  * The trait can be serialized and the implementation itself is responsible for
  * storing the parameters for the transformation so that it can be used
  * to modify new data examples.
  *
  * Created by cd on 14/05/2016.
  */
trait ContinuousTransform extends Serializable {

  /**
    * transform the continuous matrix.
    * @param continousData
    * @return
    */
  def transform(continousData:DenseMatrix[Double]): DenseMatrix[Double]

  /**
    * already having calculated to parameters simply
    * filter the supplied data through the transformation.
    * @param data
    * @return
    */
  def filter (data:DenseMatrix[Double]):DenseMatrix[Double]
}
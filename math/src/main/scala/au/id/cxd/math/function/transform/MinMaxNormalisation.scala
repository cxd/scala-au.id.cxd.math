package au.id.cxd.math.function.transform

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * min max normalisation will use columnwise minimum and columnwise maximum to normalise the data.
  * It will store the minimum and maximum values for columns in order to be able to invert the transform.
  */
case class MinMaxNormalisation() extends ContinuousTransform {

  var colMin: DenseVector[Double] = DenseVector.ones(1)

  var colMax: DenseVector[Double] = DenseVector.ones(1)

  /**
    * transform the continuous matrix.
    *
    * @param data
    * @return
    */
  def transform(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    colMin = DenseVector.tabulate(data.cols) {
      case i => {
        val c = data(::, i)
        c.data.min
      }
    }
    colMax = DenseVector.tabulate(data.cols) {
      case i => {
        val c = data(::, i)
        c.data.max
      }
    }
    filter(data)
  }

  /**
    * already having calculated to parameters simply
    * filter the supplied data through the transformation.
    *
    * @param data
    * @return
    */
  def filter(data: DenseMatrix[Double]) =
    DenseMatrix.tabulate(data.rows, data.cols) {
      case (i, j) => {
        val minD = colMin(j)
        val maxD = colMax(j)
        (data(i, j) - minD) / (maxD - minD)
      }
    }

  /**
    * perform the inverse of the transformation
    *
    * identity transformation makes no modification to the original data
    *
    * @param data
    * @return
    */
  def invert(data: DenseMatrix[Double]): DenseMatrix[Double] =
    DenseMatrix.tabulate(data.rows, data.cols) {
      case (i, j) => {
        val minD = colMin(j)
        val maxD = colMax(j)
        (maxD - minD) * data(i, j) + minD
      }
    }
}

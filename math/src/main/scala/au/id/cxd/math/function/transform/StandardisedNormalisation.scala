package au.id.cxd.math.function.transform

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * The data will be transformed using the empircal mean and approximate variance to create
  * a spherical result.
  *
  * The standardised normalisation will store the parameters \hat{mu} and \hat{sigma}
  *
  * Created by cd on 14/05/2016.
  */
case class StandardisedNormalisation() extends ContinuousTransform {

  var meanVector:DenseVector[Double] = DenseVector.ones[Double](1)

  var sigmaVector:DenseVector[Double] = DenseVector.ones[Double](1)

  /**
    * standardize the continuous matrix.
    * $$
    * Z = \frac{X - \mu}{\sigma^2}
    * $$
    * @param data
    * @return
    */
  def transform(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    val N = data.rows
    meanVector = DenseVector.tabulate[Double](data.cols) {
      j => {
        val col = data(::,j)
        col.foldLeft(0.0) { (a, b) => a + b } / N.toDouble
      }
    }
    sigmaVector = DenseVector.tabulate[Double](data.cols) {
      j => {
        val col = data(::, j)
        val mean = meanVector(j)
        col.foldLeft(0.0) { (a, b) => a + Math.pow(b - mean, 2) } / (N - 1).toDouble
      }
    }
    filter(data)
  }

  /**
    * already having calculated to parameters simply
    * filter the supplied data through the transformation.
    * @param data
    * @return
    */
  def filter (data:DenseMatrix[Double]):DenseMatrix[Double] = {
    val Z = DenseMatrix.tabulate[Double](data.rows, data.cols) {
      case (i, j) => {
        val x = data(i,j)
        val mean = meanVector(j)
        val sigma = sigmaVector(j)
        val s = Math.sqrt(sigma)
        val z = if (s.isNaN)
          x
        else (x - mean) / Math.sqrt(sigma)
        z
      }
    }
    Z
  }

  /**
    * perform the inverse of the standard normal transformation
    * @param data
    * @return
    */
  def invert(data:DenseMatrix[Double]):DenseMatrix[Double] = {
    val X = DenseMatrix.tabulate[Double](data.rows, data.cols) {
      case (i, j) => {
        val z = data(i,j)
        val mean = meanVector(j)
        val sigma = sigmaVector(j)
        val x = Math.sqrt(sigma)*z + mean
        x
      }
    }
    X
  }
}

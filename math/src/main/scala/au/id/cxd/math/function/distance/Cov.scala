package au.id.cxd.math.function.distance

import java.security.InvalidParameterException

import au.id.cxd.math.function.column.ColMeans
import breeze.linalg.DenseMatrix

/**
  * calculate the variance covariance between two matrices of conforming dimensions
  *
  * The number of rows and A and B should be the same, as well as A and B having the same number of columns.
  *
  * The result is a square matrix.
  *
  * Note if the matrix is scaled before hand this is equivalent to the correlation matrix.
  *
  * @param a
  * @param b
  */
class Cov (a:DenseMatrix[Double], b:DenseMatrix[Double]) {
  protected  def deltaMat (m:DenseMatrix[Double]) = {
    val means = ColMeans (m)
    val delta = m.mapPairs {
      (ind, value) =>
        val mu = means(0,ind._2)
        Math.pow(value - mu, 2.0)
    }
    delta
  }

  def op():DenseMatrix[Double] = {
    val dA = deltaMat (a)
    val dB = deltaMat (b)
    val C = dA.t * dB
    val n = dA.rows.toDouble
    C / (n - 1)
  }
}
object Cov {
  def apply(a:DenseMatrix[Double]) =
    new Cov(a, a).op()

  def apply(a:DenseMatrix[Double], b:DenseMatrix[Double]) = {
    a.rows == b.rows && a.cols == b.cols match {
      case true => new Cov(a,b).op()
      case _ => throw new InvalidParameterException("Dimensions of A and B must match.")
    }
  }
}
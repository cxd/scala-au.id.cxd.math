package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, inv, DenseVector}

/**
 * Least squares is a simpler form of regression.
 *
 * $\beta$ is estimated using the sample instances and the sample outputs as shown
 *
 * $$
 *   \hat{\beta} = (X'X)^{-1}X'Y
 * $$
 *
 * Created by cd on 18/10/14.
 */
class LeastSquares(X:DenseMatrix[Double], Y:DenseVector[Double]) {

  var B:DenseVector[Double] = DenseVector.ones(Y.size);

  var X1:DenseMatrix[Double] = DenseMatrix.horzcat(DenseMatrix.ones[Double](X.rows, 1), X)

  /**
   * train the least squares to produce the estimator for beta
   */
  def train() = {
    val C = inv(X1.t * X1)
    val D = X1.t*Y
    B = C*D
    B
  }

  /**
   * predict the result of multiplying the beta estimater against a new vector X
   * note the length of X must equal the length of Beta
   * @param x
   * @return
   */
  def predict(x:DenseVector[Double]) = {
    val y1 = B.t * x
    y1
  }

}

object LeastSquares {
  def apply(X:DenseMatrix[Double], Y:DenseVector[Double]) =
    new LeastSquares(X,Y)
}
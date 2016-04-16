package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, inv, DenseVector}

/**
  * ##import MathJax
  *
  * Least squares is a simpler form of regression.
  *
  * $\beta$ is estimated using the sample instances and the sample outputs as shown
  *
  * $$
  * \hat{\beta} = (X'X)^{-1}X'Y
  * $$
  *
  * Created by cd on 18/10/14.
  */
class LeastSquares(X: DenseMatrix[Double], Y: DenseVector[Double]) {

  /**
    * The beta parameter is approximated by
    * $$
    * \hat{\beta} = (X'X)^{-1}X'Y
    * $$
    *
    * Note we assume that $\beta$ is normally distributed as
    *
    *
    *
    * $$
    * \hat{\beta} = N( \beta, Q \sigma^2 )
    * $$
    *
    * where $Q = (X'X)^{-1}$
    *
    * $\hat{\beta}$ is the Maximum likelihood estimate of $\beta$.
    *
    *
    *
    */
  var Beta: DenseVector[Double] = DenseVector.ones(Y.size);


  /**
    * The variance of beta is given  by
    *
    * $$
    * Var(\beta) = Q \sigma^2
    * $$
    *
    * with $Q = (X'X)^{-1}$
    *
    * The variance parameter for $\sigma^2$ is approximated as
    *
    * $$
    * \frac{1}{N - p - 1} \sum (y_i - \hat{y_i})^2
    * $$
    *
    * This value is computed during the training cycle.
    */
  var betaVariance: DenseMatrix[Double] = DenseMatrix.zeros(X.cols, X.cols)

  /**
    * The variance parameter for $\sigma^2$ is approximated as $s^2$
    *
    * $$
    * \frac{1}{N - p - 1} \sum_{i=1}^N (y_i - \hat{y_i})^2
    * $$
    *
    * This value is computed during the training cycle.
    */
  var variance: Double = 0

  var X1: DenseMatrix[Double] = DenseMatrix.horzcat(DenseMatrix.ones[Double](X.rows, 1), X)

  /**
    * train the least squares to produce the estimator for beta
    */
  def train() = {
    val C = inv(X1.t * X1)
    val D = X1.t * Y
    Beta = C * D

    val yHat = predict(X)
    val yDelta = (Y - yHat).foldLeft(0.0d) {
      (total, delta) => total + Math.pow(delta, 2.0)
    }
    // form the estimate of the beta variance
    variance = 1.0 / (Y.size - X.cols - 1) * yDelta
    betaVariance = variance * C

    Beta
  }

  /**
    * predict the result of multiplying the beta estimater against a new vector X
    * note the length of X must equal the length of Beta
    *
    * $$
    *   \hat{Y} = \beta_0 + \sum X_i \beta_j
    * $$
    *
    * $$
    *   \hat{Y} = X\beta
    * $$
    *
    * @param x
    * @return
    */
  def predict(x: DenseMatrix[Double]): DenseVector[Double] = {
    val xBias = DenseMatrix.horzcat(DenseMatrix.ones[Double](X.rows, 1), x)
    val yHat = Beta.t * xBias
    yHat.inner
  }

}

object LeastSquares {
  def apply(X: DenseMatrix[Double], Y: DenseVector[Double]) =
    new LeastSquares(X, Y)
}
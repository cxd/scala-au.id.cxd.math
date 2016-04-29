package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, inv, DenseVector}

/**
  * ##import MathJax
  *
  * Least squares is a simpler form of regression approximating $Y$ as
  *
  * $$
  *   \hat{Y} = \beta_0 + \sum X_i \beta_j
  * $$
  *
  * $$
  *   \hat{Y} = X\beta
  * $$
  *
  * The parameter $\beta$ is estimated using the sample instances and the sample outputs as shown
  *
  * $$
  * \hat{\beta} = (X'X)^{-1}X'Y
  * $$
  *
  * $\hat{\beta}$ is assumed to have a normal distribution with mean $\beta$
  *
  * and variance $Q \sigma^2$
  *
  * where $Q = (X'X)^{-1}$.
  *
  * The residual squared error can be calculated as:
  *
  * $$
  * RSS(\beta) = \sum (Y_i - \hat{Y_i})^2
  * $$
  *
  * The residuals $\epsilon$ are assumed distributed as $N(0,\sigma^2)$
  *
  * Inference on $\beta$ can be performed using the standardised coefficient z-score for $\beta$.
  *
  * $$
  * z_j = \frac{\hat{\beta_j}}{\hat{\sigma}\sqrt{v_j}}
  * $$
  *
  * The value for $v_j$ is derived from the $jth$ position on diagonal from the matrix $(X'X)^{-1}$.
  *
  * The Z-score from the normal distribution at a corresponding alpha level can be used to form
  * the associated confidence interval for $\beta$ at $p-value = z$ at the $1 - \alpha$ level .
  *
  *
  * $$
  * \hat{\beta_j} \pm z^{(1-\alpha)} \sqrt{v} \hat{\sigma}
  * $$
  *
  * As $\beta$ defines the coefficients of the $pth$ attribute in $X$ it is possible to test whether
  * the $kth$ coefficient can be set to $0$ (in which case the contribution of $X_k$ to estimating $Y$ is not significant)
  * by using an F-score.
  * Let $k_1$ equal the $k$ parameters and $k_0$ be the a smaller model where $k_1 - k_0$ parameters are set to $0$
  * the F-score can be calculated as
  *
  * $$
  *   F = \frac{(RSS_0 - RSS_1)/(k_1 - k_0)}{RSS_1/(N - k_1 - 1)}
  * $$
  *
  * this statistic can be used to determine if the residual sum of squares error is changed significantly
  * by setting the $k_1 - k_0$ parameters to 0. If the RSS decreases, and the F-score can be tested
  * against a corresponding p-value for an associated $\alpha$ level to determine if the improvement
  * is significant change. If so, the corresponding attributes contribution in determining $Y$ is marginal.
  *
  * For further details refer to
  *
  * Hastie, T. Tibshirani, R. Friedman, J. The Elements of Statistical Learning, Second Ed. Springer 2009.
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
    * The variance parameter for $\sigma^2$ is approximated as
    *
    * $$
    * \frac{1}{N - p - 1} \sum (y_i - \hat{y_i})^2
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
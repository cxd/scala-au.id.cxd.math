package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * The bayes logistic least squares mixes in the
  * logistic transform and estimation functions with the
  * underlying regression method provided by the bayesian linear regressor.
  *
  * This enables the addition of the "update" method to generate the posterior distributions.
  *
  * Created by cd on 18/06/2016.
  */
class BayesLogisticLeastSquares(@transient var inX1: DenseMatrix[Double], @transient var inY1: DenseVector[Double], override val m: Int = 1)
  extends BayesLinearRegression(inX1, inY1, m) with LogisticRegressor {

}
object BayesLogisticLeastSquares {
  /**
    * generate a new ols model for supplied X and Y values at the corresponding degree m
    *
    * @param X
    * @param Y
    * @param m
    * @return
    */
  def apply(X: DenseMatrix[Double], Y: DenseVector[Double], m: Int) = {
    val ols = new BayesLogisticLeastSquares(X, Y, m)
    ols
  }
}


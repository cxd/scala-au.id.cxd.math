package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * ##import MathJax
  * The logistic least squares implements the logistic regression using the
  * ordinary least squares as the regression method.
  *
  * The logistic regression provides the estimate of $P(y|X)$ which determines the probability
  * that the data point is a member of either class $$y \in 0,1$$ for the predictor variables.
  *
  * $$
  * P(y = 1|X) = \frac{1}{\left( 1 + e&#94;{(-\beta_0 + \sum_{k} \beta_k x_k )} \right)}
  * $$
  *
  * conversely
  *
  * $$
  * P(y = 0|X) = \frac{ e&#94;{(-\beta_0 + \sum_{k} \beta_k x_k ) }{ { ( 1 + e&#94;{ (-\beta_0 + \sum_{k} \beta_k x_k ) } ) }
  * $$
  *
  * The logit function for the odds ratio is the transformation of the above and is estimated as
  *
  * $$
  * log \frac{P(y=0|X)}{P(y=1|X)} = \beta_0 + \beta&#94;T X
  * $$
  *
  * The logistic regression proceeds by estimating the logit function, once estimated the class probability
  * can be estimated using the logistic function.
  *
  * This implementation mixes in the LogisticRegressor trait with the OrdLeastSquares implementation.
  *
  * Created by cd on 1/05/2016.
  */
class LogisticLeastSquares(@transient var inX: DenseMatrix[Double], @transient var inY: DenseVector[Double], override val m: Int = 1)
  extends OrdLeastSquares(inX, inY, m) with LogisticRegressor {

}

object LogisticLeastSquares {
  /**
    * generate a new ols model for supplied X and Y values at the corresponding degree m
    *
    * @param X
    * @param Y
    * @param m
    * @return
    */
  def apply(X: DenseMatrix[Double], Y: DenseVector[Double], m: Int) = {
    val ols = new LogisticLeastSquares(X, Y, m)
    ols
  }
}

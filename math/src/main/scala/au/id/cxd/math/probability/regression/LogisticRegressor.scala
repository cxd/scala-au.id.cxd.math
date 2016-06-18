package au.id.cxd.math.probability.regression

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * The logistic regressor is layered ontop of the family of least squares regressors
  * the output function is modified by means of the logistic function.
  *
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
  * Created by cd on 18/06/2016.
  */
trait LogisticRegressor {
  this : OrdLeastSquares =>

  /**
    * the conversion function can be used to transform
    * the logit function into the probabilities and class assignments
    * for the 2 class problem.
    *
    * @param logitY
    * @return
    */
  def transform(logitY:DenseVector[Double]) = {
    val resultP = DenseMatrix.tabulate[Double](logitY.length, 3) {
      case (i, j) => {
        val y = logitY(i)
        val p0 = Math.exp(-y) / (1.0 + Math.exp(-y))
        val p1 = 1.0 / (1.0 + Math.exp(-y))

        j match {
          case 0 => {
            p0
          }
          case 1 => {
            p1
          }
          case _ => {
            val classY = p0 > p1 match {
              case true => 0.0
              case _ => 1.0
            }
            classY
          }
        }
      }
    }
    resultP
  }

  /**
    * produces the estimates for $P(y|X)$ for a two class classification problem.
    * Returns the estimates in a 3 column matrix of n rows
    * The third column classifies as either 0 or 1 depending on the maximum value.
    *
    * @param X1
    *
    * @return (DenseVector[Double], DenseMatrix[Double])
    *
    * The tuple that is returned contains the estimate $B'X$ as the first value
    * and the second value is a three column matrix containing the probabilities
    * of class 0 and class 1 in the first  two columns and the classification 1 or 0
    * in the third column.
    *
    *
    */
  def estimate(X1: DenseMatrix[Double]):(DenseVector[Double], DenseMatrix[Double]) = {
    // predict the logistic function produces a single row matrix with each column corresponding to the estimate.
    val logitY = predict(X1) .toDenseVector
    (logitY, transform (logitY))
  }

}

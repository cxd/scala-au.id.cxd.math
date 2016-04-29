package au.id.cxd.math.examples.probability.regression

import au.id.cxd.math.function.PolynomialExpansion
import breeze.linalg._
import breeze.numerics.pow

/**
  * ##import MathJax
  *
  * Created by cd on 28/06/2014.
  * A class to perform regression by means of ordinary least squares.
  * This is the same approach as that taken in the [[LeastSquares]] approach,
  * with slightly different parameterisation.
  * In this case the degree of the polynomial is supplied to the constructure
  * and the input vector has features added for the degree of the polynomial during training and prediction.
  */
class OrdLeastSquares(X: DenseMatrix[Double], Y: DenseVector[Double], m: Int = 1, threshold: Double = 0.5) {

  /**
    * initial weight vector up to size M
    */
  var W = DenseMatrix.ones[Double](m + 1, 1)

  val powers = for {
    i <- 0 to m
  } yield i.toDouble

  var P = createPolynomial(X, m)

  var Y1 = DenseMatrix.tabulate(Y.length, 1) { case (i, j) => Y(i) }

  /**
    * first determine how many features will be generated
    * for the number of columns.
    *
    * For example if we have cols 2 and degree m = 2
    * (a, b) will have (a, b, a&#94;2, 2ab, b&#94;2)  which is 3 extra features
    *
    * $$
    * (a, b, c)&#94;n = (a, b, b, a&#94;2, ab, b&#94;2, bc, c&#94;2, x&#94;3, x&#94;2b, x&#94;2c, ab&#94;2, abc, ac&#94;2, b&#94;3, c&#94;3, ..., )
    * $$
    *
    * We can use the multinomial theorem to do the expansion
    *
    * the coefficients up to n can be determined using the rule
    *
    * $$
    *   \left( n \choose {k_1, k_2, k_3, ..., k_m} \right)
    * $$
    *
    * where for any combination of $k$ $\sum_{i=1}&#94;m k$ must equal $n$.
    *
    * $m$ represents the number of columns in matrix $X$ and has
    *
    */

  def createPolynomial(X1: DenseMatrix[Double], m: Int): DenseMatrix[Double] = {
    PolynomialExpansion(X1, m)
  }

  /**
    *
    *
    * http://brisbanepowerhouse.org/events/2016/06/30/daas-near-death-experience/
    * multiply the factor vector by the
    *
    * @param B beta matrix.
    * @param X1 matrix of factors
    */
  def multWeights(B: DenseMatrix[Double], X1:DenseMatrix[Double]): DenseMatrix[Double] = {
    println(s"B dim = ${B.rows} x ${B.cols}")
    println(s"X dim = ${X1.rows} x ${X1.cols}")

    B * X1.t
  }

  /**
    * calculate the squared error between the target column vector and the function column vector
    * vectors must have same dimension
    *
    * @param T
    * @param Y
    */
  def squaredError(T: DenseMatrix[Double], Y: DenseMatrix[Double]): Double = {
    val delta = pow((Y - T), 2.0)
    val total = sum(delta)
    total.asInstanceOf[Double] * 0.5
  }

  /**
    * update the weight matrix using the inverse of the covariance matrix and the residuals
    */
  def updateWeights(F: DenseMatrix[Double], Y: DenseMatrix[Double]): DenseMatrix[Double] = {
    val Cov = F.t * F
    val I = pinv(Cov)
    val B = (I * F.t)
    println(s"B dim ${B.rows} x ${B.cols}")
    println(s"Y dim ${Y.rows} x ${Y.cols}")
    Y*B.t
  }

  /**
    * train the least squares model.
    */
  def train(): Tuple2[DenseMatrix[Double], Double] = {
    W = updateWeights(P, Y1)
    val T = multWeights(W, P)
    val error = squaredError(T, Y1)
    (T, error)
  }

  /**
    * having trained the model predict a value for the new x input.
    *
    * @param x
    */
  def predict(x: Double) = {
    // convert to a polynomial
    // original column size: X1.cols
    val cols = P.cols
    val X1 = DenseMatrix.tabulate[Double](1, cols){
      case (i, j) => Math.pow(x, i)
    }
    // TODO: check use of single value
    multWeights(W,X1)
  }

  def predictSeq(x: DenseVector[Double]) = {
    val M = DenseMatrix.tabulate[Double](1,x.length) {
      case (i, j) => x(j)
    }
    val M1 = createPolynomial(M, m)
    multWeights(W,M1)
  }

}

object OrdLeastSquares {

  def apply(X: DenseMatrix[Double], Y: DenseVector[Double], m: Int, threshold: Double = 0.5) = {
    val ols = new OrdLeastSquares(X, Y, m, threshold)
    ols
  }

}

package au.id.cxd.math.probability.regression

import breeze.linalg._
import breeze.numerics.pow

/**
 * Created by cd on 28/06/2014.
 * A class to perform regression by means of ordinary least squares.
 */
class OrdLeastSquares(X: DenseVector[Double], Y: DenseVector[Double], m: Int = 1, threshold: Double = 0.5) {

  /**
   * initial weight vector up to size M
   */
  var W = DenseVector.ones[Double](m+1)

  val powers = for {
    i <- 0 to m
  } yield i.toDouble

  var P = DenseMatrix.ones[Double](X.size, m+1)

  /**
   * initialise the collection of powers
   */
  def init() {
    val pair = X.foldLeft(Tuple2[DenseMatrix[Double], Int](P, 0)) {
      (pair, x: Double) => {
        val tuple = pair.asInstanceOf[Tuple2[DenseMatrix[Double], Int]]
        val P1 = tuple._1
        val powerX = DenseVector[Double]((for {i <- 0 to m} yield Math.pow(x, i)).toArray)
        P1(tuple._2, ::) := powerX.t
        (P1, tuple._2 + 1)
      }
    }
    P = pair.asInstanceOf[Tuple2[DenseMatrix[Double], Int]]._1
  }

  /**
   * multiply the factor vector by the
   * @param F matrix of factors
   */
  def multWeights(F: DenseMatrix[Double]):DenseVector[Double] = {
    P * W
  }

  /**
   * calculate the squared error between the target column vector and the function column vector
   * vectors must have same dimension
   * @param T
   * @param Y
   */
  def squaredError(T:DenseVector[Double], Y:DenseVector[Double]):Double = {
    val delta = pow((Y-T), 2.0)
    val total = sum(delta)
    total.asInstanceOf[Double] * 0.5
  }

  /**
   * update the weight matrix using the inverse of the covariance matrix and the residuals
   */
  def updateWeights(F:DenseMatrix[Double], T:DenseVector[Double], Y:DenseVector[Double]):DenseVector[Double] = {
    val Cov = F.t * F
    val I = inv(Cov)
    val B = (I * F.t).asInstanceOf[DenseMatrix[Double]]
    B * Y
  }

  /**
   * train the least squares model.
   */
  def train():Tuple2[DenseVector[Double], Double] = {
    val T = multWeights(P)
    val error = squaredError(T, Y)
    if (error < threshold) {
      (T, error)
    } else {
      W = updateWeights(P, T, Y)
      train()
    }
  }

  /**
   * having trained the model predict a value for the new x input.
   * @param x
   */
  def predict(x:Double) = {
    // convert to a polynomial
    val X = DenseVector((for {i <- 0 to m} yield Math.pow(x, i)).toArray)
    val M = DenseMatrix.ones[Double](1, X.size)
    M(1,::) := X.t
    multWeights(M)
  }

}

object OrdLeastSquares {

  def apply(X: DenseVector[Double], Y: DenseVector[Double], m: Int, threshold: Double = 0.5) = {
    val ols = new OrdLeastSquares(X, Y, m, threshold)
    ols.init()
    ols
  }

}

package au.id.cxd.math.function.distance

import au.id.cxd.math.function.column.ColMeans
import breeze.linalg.{DenseMatrix, DenseVector, diag, inv, tile, trace}

/**
  * ##import MathJax
  *
  * Mahalanobis Distance
  * An elliptical distance measure for the difference between two samples having the spread of their covariance matrix.
  *
  * Given samples X and Y the Mahalanobis distance is given as
  *
  * $$
  * dist(X,Y) = \left\{ (X-Y)' \Sigma_{X,Y}&#94;{-1} (X-Y) \right\}&#94;{1/2}
  * $$
  * Where $\Sigma_{X,Y}&#94;{-1}$ is the inverse of the variance-covariance matrix.
  *
  * This bears a relationship to the shape and location of a multivariate normal distribution where the $Y$ sample in
  * this case takes the place of the $\mu$ parameter for the mean of $X$ and the variance-covariance matrix is formed by the covariates of $X$.
  *
  * This class supports distance calculations for
  *
  * - The instance of two vectors $X$ and $Y$ the result is a single value
  *
  * - The instance of two Matrices $X$ and $Y$ the result is a vector where each item is the distance of each row of X to each row of Y
  *
  * - And in the case of a single matrix where the parameters $\mu$ and $\Sigma$ are estimated, where each item is the distance from X to
  * the distribution parameters given or the parameters estimated from X,
  * note in the case of the single matrix the resulting vector can then be sorted and plotted against the quartiles of the chi-squared distribution
  * in order to define a qqplot, rendering a visualisation of the deviation from the expected quartile under the Multivariate normal assumption.
  * This type of visualisation is useful in diagnosing whether a data set deviates from MVN.
  *
  */
class MahalanobisDistance() {

  /**
    * after the distance calculation keep the full distance matrix.
    */
  var distMatrix:DenseMatrix[Double] = DenseMatrix.zeros(1,1)


  def mahalanobis(delta:DenseMatrix[Double], sigma:DenseMatrix[Double]):DenseMatrix[Double] = {
    val m = if (delta.rows == 1 || delta.cols == 1) delta
            else delta.t
    val sigmaInv = if (sigma.rows == 1 && sigma.cols == 1) {
      sigma.map{ v => if (v != 0.0) 1.0/v else 0.0 }
    } else inv(sigma)
    val d = m.t * (sigmaInv * m)
    distMatrix = d
    diag(d).toDenseMatrix.map(Math.sqrt)
  }


  /**
    * a distance operation where the parameters Mu and Sigma have already been estimated.
    * @param X
    * @param Mu
    * @param Sigma
    * @return
    */
  def dist(X:DenseMatrix[Double], Mu:DenseMatrix[Double], Sigma:DenseMatrix[Double]):DenseMatrix[Double] = {
    val Delta = X-Mu
    mahalanobis(Delta, Sigma)
  }

  /**
    * distance between two matrices
    * @param X
    * @param Y
    * @return
    */
  def dist(X:DenseMatrix[Double], Y:DenseMatrix[Double]):DenseMatrix[Double] = {
    val Sigma = Cov(X,Y)
    val Delta = X - Y
    mahalanobis(Delta, Sigma)
  }

  /**
    * determine the distance from the mean using the variance covariance matrix of the supplied sample
    * @param X
    */
  def dist(X:DenseMatrix[Double]) = {
    val Sigma = Cov(X,X)
    val colMean = ColMeans(X)
    val Mean = DenseMatrix.tabulate(X.rows, X.cols) {
      case (i,j) => colMean(0,j)
    }
    val Delta = X - Mean
    mahalanobis(Delta, Sigma)
  }

  /**
    * mahalanobis distance between two vectors.
    * @param X
    * @param Y
    * @return
    */
  def dist(X:DenseVector[Double], Y:DenseVector[Double]):Double = {
    val xMat = X.asDenseMatrix
    val yMat = Y.asDenseMatrix
    val Delta = xMat - yMat
    val Sigma = Cov(xMat,yMat)
    val d = mahalanobis(Delta,Sigma)
    // in this case the distance between two vectors is a single value.
    d(0,0)
  }

}
object MahalanobisDistance {
  def apply(X:DenseVector[Double], Y:DenseVector[Double]):Double =
    new MahalanobisDistance().dist(X,Y)

  def apply(X:DenseMatrix[Double], Y:DenseMatrix[Double]):DenseMatrix[Double] =
    new MahalanobisDistance().dist(X,Y)

  def apply(X:DenseMatrix[Double]):DenseMatrix[Double] =
    new MahalanobisDistance().dist(X)

  def apply(X:DenseMatrix[Double], Mu:DenseMatrix[Double], Sigma:DenseMatrix[Double]):DenseMatrix[Double] =
    new MahalanobisDistance().dist(X,Mu,Sigma)
}

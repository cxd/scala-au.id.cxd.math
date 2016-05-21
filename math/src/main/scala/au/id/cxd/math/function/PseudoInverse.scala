package au.id.cxd.math.function

import breeze.linalg.{DenseMatrix, DenseVector, svd}

/**
  * Compute the pseudo inverse of the supplied square matrix
  *
  * Created by cd on 21/05/2016.
  */
class PseudoInverse(val P: DenseMatrix[Double], val correction: Double = Math.E) {

  /**
    * compute the pseudo inverse using SVD.
    *
    * However if the pseudo inverse contains negative eigenvectors the correct for this
    * by adding a small constant to the diagonal of the input matrix.
    *
    * As mentioned in the link below, if the model contains negative eigenvalues adding a small constant $\lambda$
    * to the diagonal of the covariance matrix is a way of adding noise to the model, by indicating that the variance
    * is larger than expected in the original covariance matrix.
    *
    * See: http://stats.stackexchange.com/questions/50947/numerical-instability-of-calculating-inverse-covariance-matrix
    *
    * One reason for the negative eigenvector is either the input matrix is non-singular
    * or potentially there is some overflow for the double value.
    *
    * Note the wikipedia page describes the ridge regression as having similar properties to the
    * method of adding the constant to the variance.
    *
    * https://en.wikipedia.org/wiki/Tikhonov_regularization
    *
    *
    */
  def op() = {
    P.rows != P.cols match {
      case true => throw new Exception(s"Supplied matrix is not square, supply square matrix")
      case false => {
        val (diag, inverse) = invert(P)
        containsNegative(diag) match {
          case true => {
            val C = adjust(P, correction)
            val (diag2, inverse2) = invert (C)
            inverse2
          }
          case _ => inverse
        }

      }
    }


  }

  /**
    * adjust the covariance matrix by adding a small constant.
    *
    * This adjustment adds a small value to the variance
    *
    * @param P
    * @param c
    * @return
    */
  def adjust(P:DenseMatrix[Double], c:Double) = DenseMatrix.tabulate[Double](P.rows, P.cols) {
    case (i, j) => (i == j) match {
      case true => P(i, j) + c
      case _ => P(i, j)
    }
  }

  /**
    * determine if the diagonal contains a negative value
    *
    * @param diag
    */
  def containsNegative(diag: DenseVector[Double]) = {
    val neg = diag.findAll { f => f < 0 }
    neg.length > 0
  }

  /**
    * A = U \Sigma V'
    */
  def invert(M: DenseMatrix[Double]) = {
    val svd.SVD(u, sigma, vt) = svd(M)
    // max S^{-1} the diagonal only
    val sigmaInvert = DenseMatrix.tabulate[Double](u.rows, u.cols) {
      case (i, j) => {
        i == j match {
          case true => {
            val v = sigma(i)
            if (v < 0) {
              println(s"diagonal $i les than 0 where $v < 0")
            }
            if (v != 0) {
              1 / v
            } else {
              0.0
            }
          }
          case _ => 0.0
        }
      }
    }
    val pInverse = vt.t * sigmaInvert * u.t
    val diag = DenseVector.tabulate[Double](pInverse.cols) {
      (i) => pInverse(i, i)
    }
    (diag, pInverse)
  }

}

object PseudoInverse {
  def apply(P:DenseMatrix[Double]) = new PseudoInverse(P).op()

  def apply(P:DenseMatrix[Double], correction:Double) = new PseudoInverse(P, correction).op()
}
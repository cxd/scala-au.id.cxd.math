package au.id.cxd.math.model.components

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector, svd}

/**
  * ##import MathJax
  *
  * The singular value decomposition provides a decomposition of a matrix such that
  *
  * $$
  * A = USV'
  * $$
  *
  * The decomposition is described in Skillicorn 2007, as one which indicates the amount of variation in the
  * latent features of the data set.
  *
  * Several interpretations are explained in the book giving the following views
  *
  * - Factor interpretation
  * The rows of $V'$ are interpreted as underlying factors. The transformed basis provides a set of new factors
  * a correlation between the original attributes of A and the rows of V can be constructed in order to determine
  * the relationship between the original attributes and the factors in V. This relationship may include positive and
  * negative relationships for multiple attributes with a single factor in $V'$.
  *
  *
  * - Geometric interpretation
  * The interpretation of the components is similar to the interpretation of PCA in this approach.
  * When reviewing the relationship of of objects to attributes the $V'$ matrix can be seen as an orthonormal basis for the matrix $U$
  * Alternately, $U$ also represents an orthonormal basis for the matrix $V'$.
  *
  * This is especially useful for visualisation. For example, coordinates from $U$ can be plotted,
  * these are separated by the directions of the corresponding components in $V'$.
  *
  *
  * - Graphical interpretation
  *
  * The resulting matrices describe the following
  * The matrix $U$ describes the relationship between objects or rows of A, the transpose matrix $V'$ describes the
  * relationship between attributes or columns in $A$ with the rows of $V'$ or columns of $V$ its transpose.
  * The diagonal (or vector) $S$ describes the amount of variation in the respective components which are the columns of $U$
  * and the columns of $V'$.
  * A useful difference between PCA and SVD is that whilst PCA is capable of measuring either the relationship between objects $(A'A)$
  * or the relationship between attributes $AA'$ it can only do so distinctly, and not both at the same time in the single operation.
  * Whilst the SVD can provide the measurements of components for objects and attributes in one operation.
  *
  * Data is considered to be standardised and spherically distributed prior to executing the procedure, due to this
  * an appropriate method of normalisation or standardisation should be performed on the input data set prior to
  * performing the procedure.
  *
  * There are some exceptions to this rule as in the case of matrices of dummy variables for example.
  *
  * The $S$ matrix in the components interpretation provides a measure of the amount of variation each component c
  *
  *
  * References:
  *
  * 1. Skillicorn, D. Understanding Complex Datasets: Data Mining with Matrix Decompositions. Chapman and Hall/CRC 2007
  *
  *
  * Created by cd on 10/07/2016.
  */
class SingularValueDecomposition(P: DenseMatrix[Double]) {

  /**
    * perform the svd operation.
    *
    * @return
    */
  def op() = {
    val svD = svd(P)
    svD
  }


  def singularDiagonal(svD: SVD[DenseMatrix[Double], DenseVector[Double]]) =
    DenseMatrix.tabulate[Double](svD.S.length, svD.S.length) {
      (i, j) =>
        i == j match {
          case true => svD.S(i)
          case _ => 0.0
        }
    }

  /**
    * calculate the contribution of each singular vector to the whole entropy
    * for each singular component calculated as
    *
    * $$
    * f_k = s_k&#94;2/\sum_{i=1}&#94;r s_i&#94;2
    * $$
    *
    * @param svD
    * @return vector f of values for contribution to entropy.
    */
  def contributions(svD: SVD[DenseMatrix[Double], DenseVector[Double]]) = {
    val sumSquare = svD.S.foldLeft(0.0) {
      (total, s) => total + Math.pow(s, 2.0)
    }
    val f = svD.S.map { s => Math.pow(s, 2.0) / sumSquare }
    f
  }

  /**
    * calculate the entropy for the entire data set.
    * Based on the singular components this gives a value between 0 and 1 which indicates
    * the spread of variation within the singular components.
    * If the entropy is close to 0 the spread of variation is explained by the first singular component
    * if the entropy is close to 1 the spread of variation is almost uniform between all singular components.
    *
    * @param svD
    * @return
    */
  def entropy(svD: SVD[DenseMatrix[Double], DenseVector[Double]]) = {
    val f = contributions(svD)
    val c = -1.0 / Math.log(f.length)
    val sumF = f.foldLeft(0.0) {
      (total, fk) => {
        total + fk * Math.log(fk)
      }
    }
    val entropy = c * sumF
    entropy
  }


  /**
    * approximate correlation matrix of object (rows) and attributes (columns)
    * is derived from the SVD decomposition
    * $$
    * X = U\SigmaV'
    * $$
    * such that
    * $$
    * XX' = U\Sigma&#94;2U'
    * $$
    * Where $\Sigma$ is the diagonal matrix of the singular values.
    *
    * @param svD
    */
  def objectAttribCorrelation(svD: SVD[DenseMatrix[Double], DenseVector[Double]]) = {
    val sMat = singularDiagonal(svD)
    val C = svD.U * sMat * sMat.t * svD.U.t
    C
  }

  /**
    * calculate the correlation matrix of attribute to attribute (columns to columns)
    * derived from the SVD decomposition
    * $$
    *   X = U\SigmaV'
    * $$
    * The correlation matrix is approximated such that
    * $$
    *   X'X = V\Sigma&#94; V'
    * $$
    * @param svD
    * @return
    */
  def attribCorrelation(svD:SVD[DenseMatrix[Double], DenseVector[Double]]) = {
    val sMat = singularDiagonal(svD)
    val C = svD.Vt.t * sMat.t* sMat * svD.Vt
    C
  }

}


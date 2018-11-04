package au.id.cxd.math.model.components

import au.id.cxd.math.function.distance.Cor
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg.eigSym.EigSym
import breeze.linalg.{DenseMatrix, DenseVector, eig, eigSym}

/**
  * ##import MathJax
  *
  *
  *
  * implement operations to support PCA
  *
  * generate the PCA of the correlation matrix for the sphereised data.
  *
  * The opertion returns a tuple of:
  *
  * (eigenValues, eigenVectors, varExplained, projection)
  *
  * Created by cd on 10/07/2016.
  */
class PrincipleComponentsAnalysis(scale:Boolean=true) {

  def op(X: DenseMatrix[Double]) = {
    var x1 =  if (scale) StandardisedNormalisation().transform(X)
              else X
    val corMat = Cor(x1)

    val (eigenValues, eigenVectors, varExplained) = EigenDecomposition(corMat)

    // now we can project the original data into the eigen vector
    val projection = x1 * eigenVectors

    (eigenValues, eigenVectors, varExplained, projection)
  }

}
object PrincipleComponentsAnalysis {

  /**
    * determine the percent of variance explained by each component.
    * @param eigenValues
    * @return
    */
  def varianceExplained(eigenValues:DenseVector[Double]): DenseVector[Double] = {
    val total = eigenValues.toArray.sum
    val percentExplained = eigenValues.map { v => v / total }
    percentExplained
  }


  /**
    * apply PCA to the supplied matrix.
    * @param X
    * @return
    *         tuple of
    *         (eigenValues, eigenVectors, variance explained, projection of X into the eigenvectors)
    */
  def apply(X:DenseMatrix[Double]): (DenseVector[Double], DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) =
    new PrincipleComponentsAnalysis().op(X)

  /**
    * apply PCA to the supplied matrix.
    * @param scale : a flag indicating whether to normalise X
    * @param X
    * @return
    * *         tuple of
    * *         (eigenValues, eigenVectors, variance explained, projection of X into the eigenvectors)
    */
  def apply(scale:Boolean, X:DenseMatrix[Double]): (DenseVector[Double], DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) =
    new PrincipleComponentsAnalysis(scale).op(X)
}

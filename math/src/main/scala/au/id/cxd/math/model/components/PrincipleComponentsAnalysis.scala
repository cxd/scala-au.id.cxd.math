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
  * Created by cd on 10/07/2016.
  */
class PrincipleComponentsAnalysis(scale:Boolean=true) {

  def op(X: DenseMatrix[Double]) = {
    var x1 =  if (scale) StandardisedNormalisation().transform(X)
              else X
    val corMat = Cor(x1)
    // find the eigendecomposition of the correlation matrix.
    val EigSym(eValues, eVectors) = eigSym(corMat)
    // note that the order of eVectors and eValues differ so we want to rearrange them.
    val n = eValues.length - 1
    val eigenValues = DenseVector.tabulate[Double](n+1) {
      case i => eValues(n-i)
    }
    val ncols = eVectors.cols - 1
    val eigenVectors = (for (i <- 0 to ncols) yield i).foldLeft(DenseMatrix.zeros[Double](eVectors.rows, eVectors.cols)) {
      (accum, j) =>
        accum(::,j) := eVectors(::, ncols - j)
        accum
    }


    // now we can project the original data into the eigen vector
    val projection = x1 * eigenVectors
    // also return the amount of variance explained by each component
    val varExplained = PrincipleComponentsAnalysis.varianceExplained(eigenValues)

    (eigenValues, eigenVectors, varExplained, projection)
  }

}
object PrincipleComponentsAnalysis {

  /**
    * determine the percent of variance explained by each component.
    * @param eigenValues
    * @return
    */
  def varianceExplained(eigenValues:DenseVector[Double]) = {
    val total = eigenValues.toArray.sum
    val percentExplained = eigenValues.map { v => v / total }
    percentExplained
  }


  def apply(X:DenseMatrix[Double]) =
    new PrincipleComponentsAnalysis().op(X)

  def apply(scale:Boolean, X:DenseMatrix[Double]) =
    new PrincipleComponentsAnalysis(scale).op(X)
}

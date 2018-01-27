package au.id.cxd.math.model.components

import au.id.cxd.math.function.distance.Cor
import au.id.cxd.math.function.transform.StandardisedNormalisation
import breeze.linalg.{DenseMatrix, DenseVector, eigSym}
import breeze.linalg.eigSym.EigSym

class EigenDecomposition {


  def op(X: DenseMatrix[Double]) = {

    // find the eigendecomposition of the correlation matrix.
    val EigSym(eValues, eVectors) = eigSym(X)
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
    // also return the amount of variance explained by each component
    val varExplained = PrincipleComponentsAnalysis.varianceExplained(eigenValues)

    (eigenValues, eigenVectors, varExplained)
  }
}
object EigenDecomposition {
  def apply(X:DenseMatrix[Double]) = {
    new EigenDecomposition().op(X)
  }
}

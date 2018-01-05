package au.id.cxd.math.model.components

import au.id.cxd.math.function.distance.Cor
import au.id.cxd.math.probability.analysis.Manova
import breeze.linalg.DenseMatrix

/**
  * ##import MathJax
  *
  * Similar in manner to the MANOVA procedure,
  * the Canonical Discriminant functions makes use of the
  * within group variation $W$ and the between group variation $B$
  * in order to solve the eigen decomposition of
  *
  * $$
  * W&#94;{-1}B
  * $$
  *
  * The resulting eigenvectors form the coefficients of the canonical functions for
  *
  * $$
  * Z_i = a_{i1}X_1 + a_{i2}X_2 + \cdots + a_{ip}X_p
  * $$
  *
  * Hence $Z_i$ is the projection of $X$ into the number of components that best describe the variability in the data.
  * The number of components is the smaller of $p$ and $n-1$.
  *
  * The value $Z_1$ is the F-ratio for a one-way analysis of variance for the variation within and between groups.
  * The subsequent discriminant function $Z_2$ represents the variation within and between groups that is not correlated with $Z_1$.
  * Subsequent discriminant functions capture the variation within and between groups not correlated with previous discriminant functions.
  *
  * The correlation between the original attributes and the canonical discriminant functions can then be used to identify which attributes
  * are represented by each of the discriminant functions. As these are a linear combination of covariates there may be more than one attribute
  * with a large magnitude of correlation with the resulting function.
  *
  * The resulting projection of the canonical functions can be used for visualisation of the ordination.
  *
  * The associated correlation matrix between attributes and components can be used to assist in the interpretation of the ordination.
  *
  * This technique can serve as a form of dimensionality reduction as well as a method for visualisation and interpretation.
  *
  */
class CanonicalDiscriminantAnalysis(groups: List[String], dataX: DenseMatrix[Double])
  extends Manova(groupNames=groups, data=dataX) {

  /**
    * extract the canonical discriminant functions and calculate the correlation between the original groups
    * and the components.
    */
  def computeZ() = {
    val n = dataX.rows
    val m = groups.size
    val p = dataX.cols
    val numComponents = List(n-1,p).min
    val T = computeT(dataX)
    val B = computeB(dataX)
    val W = T + (-1.0 * B)
    // W^{-1}B
    val WinvB = W \ B
    // compute the eigenvectors and the projection for W^{-1}B
    val (eigenValues, eigenVectors, varExplained, projections) = PrincipleComponentsAnalysis(WinvB)
    // the projections are the canonocal projections. //
    val components = eigenValues(0 until numComponents).toDenseVector
    val percentVar = varExplained(0 until numComponents).toDenseVector
    val coeffs = eigenVectors(::,0 until numComponents).toDenseMatrix
    val mat = (projections * dataX.t).t
    val zMat = mat(::,0 until numComponents).toDenseMatrix
    println(s"DIM (${zMat.rows} , ${zMat.cols})")
    // determine the correlation between the columns of the original data and the components
    val cor = Cor(dataX, mat)
    (components, coeffs, percentVar, zMat, cor)
  }

}
object CanonicalDiscriminantAnalysis {
  def apply(groups: List[String], dataX: DenseMatrix[Double]) =
    new CanonicalDiscriminantAnalysis(groups, dataX).computeZ()
}

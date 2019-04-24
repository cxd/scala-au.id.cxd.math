package au.id.cxd.math.model.components

import au.id.cxd.math.function.distance.{Cor, MahalanobisDistance}
import au.id.cxd.math.probability.analysis.Manova
import breeze.linalg.{DenseMatrix, DenseVector, diag}

import scala.collection.immutable

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
class CanonicalDiscriminantAnalysis(val classes: List[String], dataX: DenseMatrix[Double], priors:DenseVector[Double]=DenseVector.zeros[Double](0))
  extends Manova(groupNames=classes, data = dataX) {

  /**
    * extract the canonical discriminant functions and calculate the correlation between the original groups
    * and the components.
    */
  def computeZ() = {
    val n = dataX.rows
    val m = classes.size
    val p = dataX.cols
    val numComponents = List(m - 1, p).min
    val (sigma,t) = computeT(dataX)
    val T = t
    // the group means is also available
    val (betweenMat, mu) = computeB(dataX)
    val B = betweenMat
    val W = T + (-1.0 * B)
    // W^{-1}B
    val WinvB = W \ B
    // compute the eigenvectors and the projection for W^{-1}B
    val (eigenValues, eigenVectors, varExplained, projections) = PrincipleComponentsAnalysis(WinvB)

    // the projections are the canonocal projections. //
    val components = eigenValues(0 until numComponents).toDenseVector
    val percentVar = varExplained(0 until numComponents).toDenseVector
    val coeffs = eigenVectors(::, 0 until numComponents).toDenseMatrix
    val mat = (coeffs * dataX.t).t
    val zMat = mat(::, 0 until numComponents).toDenseMatrix
    println(s"DIM (${zMat.rows} , ${zMat.cols})")
    // determine the correlation between the columns of the original data and the components
    val cor = Cor(dataX, mat)

    /** consider method of calculating coefficients
      *
      * coeffs = (mu * evec) * evec'
      *
      * intercept = ( 0.5 * diag(mu * coeffs') ) + log(pi)
      *
      * where pi are the prior estimates of each class.
      *
      * assuming then that the prediction is
      *
      * log phat = intercept + coeffs X
      *
      * the most likely class is then the max log phat
      *
      * The discriminant function is then
      * $$
      * g_i(x) = w&#94;t_i x + w_{i0}
      * $$
      * $$
      * w_i = \Sigma&#94;{-1}\mu_i
      * $$
      * and
      * $$
      * w_{i0} = -\frac{1}{2}\mu_i'\Sigma&#94;{-1}\mu_i + \log{P (w_i))
      * $$
      */
    val groupPriors = groupSizes(classes)

    val logP = if (priors.length > 0) priors.map(p => Math.log(p))
    else {
      val p = groupPriors.map(pair => Math.log(pair._3)).toArray
      DenseVector[Double](p)
    }

    val mu1 = mu * coeffs.t
    val coeffs1 = (mu * coeffs) * coeffs.t
    // initially use equal estimate for pi, this can be derived later.
    val pi = 1.0 / mu.rows
    val intercept = (-0.5 * (diag(mu * coeffs1.t))) + logP

    (components, coeffs, intercept, percentVar, zMat, cor, mu)
  }

  /**
    * after having obtained the coefficients
    * we can project new data $Y$ into the canonical functions
    * and then do the same for the group means.
    * We then use mahalanobis distance to identify the closest corresponding group for
    * each row of the data.
    *
    * @param Y
    * the new data examples
    * @param coeffs
    * the coefficients obtained from the canonical function analysis.
    * @param groupMeans
    * the group means obtained from the original analysis.
    * each row of the group means is assumed to have been ordered alpha numerically
    * the row labels
    * @param groupLabels
    * the group labels to assign to each group.
    * this is the same length as the rows of the group column means
    * and the indexes are assumed to be in the same order as the group column means.
    * the group means is initially obtained in alpha numeric order of the original group names.
    * this parameter allows arbitrary labels to be returned instead of indexes
    * @return
    * the result is a tuple consisting of the new projection matrix for the data Y
    * the projection matrix for the group Means
    * the mahalanobis distance matrix for Y to the groupMeans for each row of Y
    * the list of indexes for the row of the group means that has the corresponding closest mahalanobis distance
    * to the corresponding row of Y
    *
    */
  def classify(Y: DenseMatrix[Double], coeffs: DenseMatrix[Double], intercept:DenseVector[Double], groupMeans: DenseMatrix[Double], groupLabels: List[String]) = {
    val groupProjection = (coeffs * groupMeans.t).t
    val yProjection = (coeffs * Y.t).t

    // now we search for the maximum discrimiant and associate the y value with that class.

    // we need to find the mahalanobis distance between each group and each row of y.
    // the distance matrix will be Y rows x group number rows
    val distances = DenseMatrix.tabulate[Double](yProjection.rows, groupProjection.rows) {
      case (i, j) => {
        val row = yProjection(i, ::)
        val group = groupProjection(j, ::)
        val dij = MahalanobisDistance(row.t, group.t)
        dij
      }
    }
    // for each row of the distance matrix we now need to identify which index is the minimum
    val groupAssignments = (for (i <- 0 until distances.rows) yield i).map {
      i => {
        val row = distances(i, ::).t.toArray
        val minD = row.map(d => Math.abs(d)).min
        val search = row.foldLeft((0, false)) {
          (accum, d) =>
            if (accum._2) accum
            else if (Math.abs(d) == minD) (accum._1, true)
            else (accum._1 + 1, false)
        }
        (search._1, groupLabels(search._1))
      }
    }
    (yProjection, groupProjection, distances, groupAssignments)
  }

  /** consider method of calculating coefficients
    *
    * coeffs = (mu * evec) * evec'
    *
    * intercept = ( 0.5 * diag(mu * coeffs') ) + log(pi)
    *
    * where pi are the prior estimates of each class.
    *
    * assuming then that the prediction is
    *
    * log phat = intercept + coeffs X
    *
    * the most likely class is then the max log phat
    *
    * The discriminant function is then
    * $$
    * g_i(x) = w&#94;t_i x + w_{i0}
    * $$
    * $$
    * w_i = \Sigma&#94;{-1}\mu_i
    * $$
    * and
    * $$
    * w_{i0} = -\frac{1}{2}\mu_i'\Sigma&#94;{-1}\mu_i + \log{P (w_i))
    * $$
    */
  def classifyDiscriminant(Y: DenseMatrix[Double], coeffs: DenseMatrix[Double], intercept:DenseVector[Double], groupMeans: DenseMatrix[Double], groupLabels: List[String]) = {
    val yProjection = (coeffs * Y.t).t
    val groupProjection = (coeffs * groupMeans.t).t

    val coeffs1 = (groupMeans * coeffs) * coeffs.t
    val wx = (coeffs1*Y.t).t
    //println(s"WX DIM (${wx.rows} x ${wx.cols})")
    val wi = DenseMatrix.tabulate[Double](wx.rows, wx.cols) {
      case (i,j) => intercept(j)
    }
    val discriminant = wi + wx
    // discriminent has m columns for each group.
    // each row of the disciminant then can be translated to a propbability
    // p represents the probabilities for each group at each row of Y.
    val p = discriminant.map { lp => Math.exp(lp) }
    // we now need to identify for each row in Y the index of the most probable group
    val groupAssignments = (for (i <- 0 until p.rows) yield i).map {
      i => {
        val row = p(i, ::).t.toArray
        val minD = row.map(d => d).max
        val search = row.foldLeft((0, false)) {
          (accum, d) =>
            if (accum._2) accum
            else if (Math.abs(d) == minD) (accum._1, true)
            else (accum._1 + 1, false)
        }
        (search._1, groupLabels(search._1))
      }
    }
    (yProjection, groupProjection, p, groupAssignments)
  }

}

object CanonicalDiscriminantAnalysis {
  def apply(groups: List[String], dataX: DenseMatrix[Double]): (DenseVector[Double], DenseMatrix[Double], DenseVector[Double], DenseVector[Double], DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]) =
    new CanonicalDiscriminantAnalysis(groups, dataX).computeZ()


  def classify(Y: DenseMatrix[Double], coeffs: DenseMatrix[Double], intercept:DenseVector[Double], groupMeans: DenseMatrix[Double], groupLabels: List[String]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], immutable.IndexedSeq[(Int, String)]) = {
    new CanonicalDiscriminantAnalysis(groupLabels, Y).classify(Y, coeffs, intercept, groupMeans, groupLabels)
  }

  def classifyDiscriminant(Y: DenseMatrix[Double], coeffs: DenseMatrix[Double], intercept:DenseVector[Double], groupMeans: DenseMatrix[Double], groupLabels: List[String]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], immutable.IndexedSeq[(Int, String)]) = {
    new CanonicalDiscriminantAnalysis(groupLabels, Y).classifyDiscriminant(Y, coeffs, intercept, groupMeans, groupLabels)
  }
}

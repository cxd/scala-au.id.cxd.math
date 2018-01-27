package au.id.cxd.math.model.components

import au.id.cxd.math.data.GroupPartition
import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.function.distance.Cov
import breeze.linalg.{*, DenseMatrix, DenseVector, diag, inv, sum, tile}

/**
  * ##import MathJax
  *
  * the quadratic discriminant function has the following assumptions.
  *
  * i) Data is multivariate normally distributed.
  * ii) Groups do not share common covariance matrix.
  *
  * This differs from the assumptions of the linear discriminant analysis in the second point.
  *
  * The resulting discriminant functions are quadratic since the original discriminant functions
  * are altered from the discriminant function used in LDA
  *
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
  *
  * To introduce the quadratic term $W_i$ giving the discriminant functions from [1] below
  *
  * $$
  * g_i(x) = x'W_ix + w_i' x + w_{i0}
  * $$
  * where
  * $$
  * W_i = -\frac{1}{2}\Sigma_i&#94;{-1}
  * $$
  * $$
  * w_i = \Sigma_i&#94;{-1}\mu_i
  * $$
  * and
  * $$
  * w_{i0} = -\frac{1}{2}\mu'_i \Sigma_i&#94;{-1}\mu_i - \frac{1}{2}\log |\Sigma_i| + \log P(\omega_i)
  * $$
  *
  * The calculation of the quadratic functions can employ the eigen decomposition of the estimate for $\hat{\Sigma}$ as follows (from [2]).
  *
  * $$
  * \hat{\Sigma_i} = U_i D_i U'_i
  * $$
  *
  * $$
  * \mu'_i \Sigma_i&#94;{-1} \mu_i = [U'_i\mu_i]'D_i&#94;{-1}[U'_i\mu_i]
  * $$
  * $$
  * (x-\hat{\mu_i})' \Sigma_i&#94;{-1} (x-\hat{\mu_i}) = [U'_i(x-\hat{\mu_i})]'D_i&#94;{-1}[U'_i(x-\hat{\mu_i})]
  * $$
  *
  * $$
  * \log|\Sigma_i| = \sum_l \log d_{il}
  * $$
  * where $d_{il}$ is the $lth$ diagonal value of the eigenvalue matrix $D_i$
  *
  * The maximum probability estimated by the discriminant function is then used to provide classification.
  *
  * This implementation provides a naive implementation of the approach.
  * The primary drawback is where the number of groups $m$ increases so too does the number of resulting decompositions.
  * Hence memory and storage can be an issue for computing the entire set of discriminant functions in one go.
  *
  * One option is to compute each decomposition separately, since the covariance matrix is estimated within groups and to
  * store those separately, rather than computing all at once.
  *
  * Note that a separate ordination may be necessary such as a princomp, svd or lda in order to visualise
  * the dimensional reduction overall without projections within each individual group.
  *
  * References:
  * 1. Duda, Richard O. Hart, Peter E. Stork, David G. (2001 ). Pattern Classification, 2nd Edition. Wiley Interscience Publication
  * 2. Hastie, T. Tibshirani, R. Friedman, J. The Elements of Statistical Learning, Second Ed. Springer 2009.
  */
class QuadraticDiscriminant(override val data: DenseMatrix[Double],
                            override val groupLabels: List[String],
                            priors: DenseVector[Double] = DenseVector.zeros[Double](0))
  extends GroupPartition {

  import QuadraticDiscriminant._

  def computeQ() = {
    val groupPriors = groupSizes(groupLabels).toIndexedSeq

    val logP = if (priors.length > 0) priors.map(p => Math.log(p))
    else {
      val p = groupPriors.map(pair => Math.log(pair._3)).toArray
      DenseVector[Double](p)
    }

    /**
      * find the index of the group prior, lookup the corresponding index in the logP
      *
      * @param name
      * @param groupPriors
      * @param logP
      */
    def groupPrior(name: String, groupPriors: IndexedSeq[(String, Int, Double)], logP: DenseVector[Double]) = {
      val idx = groupPriors.indexWhere { item => item._1.equalsIgnoreCase(name) }
      logP(idx)
    }

    partitions.map {
      pair =>
        val (groupName, groupData) = pair

        val Ci = Cov(groupData)
        val n = groupData.rows
        val mui = ColMeans(groupData)

        /**
          * $$
          * \hat{\Sigma_i} = U_i D_i U'_i
          * $$
          */
        val (eigenValues, eigenVectors, varExplained, projection) = PrincipleComponentsAnalysis(Ci)

        val lp = groupPrior(groupName, groupPriors, logP)

        /**
          * $$
          * w_{i0} = -\frac{1}{2}\mu'_i \Sigma_i&#94;{-1}\mu_i - \frac{1}{2}\log |\Sigma_i| + \log P(\omega_i)
          * $$
          */
        val intercept = lp

        (groupName, (mui, eigenValues, eigenVectors, varExplained, projection, intercept))
    }
  }


}

object QuadraticDiscriminant {


  type QuadraticDiscrimantParameters = (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double], Double)


  def apply(data: DenseMatrix[Double],
            groupLabels: List[String],
            priors: DenseVector[Double] = DenseVector.zeros[Double](0)) = {
    new QuadraticDiscriminant(data, groupLabels, priors).computeQ()
  }


  def classifyDiscriminant(Y: DenseMatrix[Double], groupParams: List[(String, QuadraticDiscrimantParameters)]) = {
    val groupNames = groupParams.map(_._1)

    def groupIdx(name: String) = groupNames.foldLeft((0, false)) {
      (pair, gname) =>
        gname.equalsIgnoreCase(name) match {
          case true => (pair._1, true)
          case _ => pair._2 match {
            case true => pair
            case _ => (pair._1 + 1, false)
          }

        }
    }

    // compute g(y) foreach group.
    val gY = groupParams.map {
      groupParam => {
        val groupName = groupParam._1
        val (mui, eV, eVecs, varExp, proj, wi0) = groupParam._2

        /**
          * $$
          * g_i(x) = x'W_ix + w_i' x + w_{i0}
          * $$
          * where
          * $$
          * W_i = -\frac{1}{2}\Sigma_i&#94;{-1}
          * $$
          * $$
          * w_i = \Sigma_i&#94;{-1}\mu_i
          * $$
          */
        val D = DenseMatrix.tabulate[Double](eV.length, eV.length) {
          case (i, j) => if (i == j) eV(i)
          else 0.0
        }
        val Dinv = inv(D)
        /**
          * $$
          * \gamma_k(x) = -\frac{1}{2} x' \Sigma_i&#94;{-1} x + x'\Sigma_i&#94;{-1} \mu_i - \frac{1}{2}\mu'_i\Sigma_i&#94;{-1}\mu_i - \frac{1}{2}\log{|\Sigma_i|} + \log \pi_i
          * $$
          */
        val logDetSigma = eV.reduce(_ + _)


        //for each group param we can iterate the rows of Y and calculate g(x) for each row.
        DenseVector.tabulate[Double](Y.rows) {
          case i => {
            val y = Y(i, ::)

            val delta = y.inner.mapPairs { case (i,v) => {
              v - mui(0,i)
            }}
            val x = eVecs.t * delta
            /**
              * $$
              * -\frac{1}{2} x' \Sigma_i&#94;{-1} x
              * $$
              */
            val Wi1 = (x.t * Dinv * x)
            /**
              * $$
              * x'\Sigma_i&#94;{-1} \mu_i
              * $$
              */
            val gi = -0.5 * Wi1 - 0.5 * logDetSigma + wi0

            gi
          }
        }
      }
    }

    // combine each vector into rows of 9 columns gY find the maximum value of p(gi|Y,params) to assign the class label.
    val mat = DenseMatrix.tabulate[Double](Y.rows, gY.length) {
      case (i, j) => {
        val g = gY(j)
        g(i)
      }
    }
    // for each row what is the maximum column
    (for (i <- 0 until Y.rows) yield i).foldLeft(List[(String, Int, Double)]()) {
      (accum, i) => {
        val scores = mat(i, ::).inner.toArray
        val maxScore = scores.max
        val maxIdx = (for (j <- 0 until scores.length) yield j).foldLeft(-1) {
          (accum2, j) => {
            scores(j) == maxScore match {
              case true => j
              case _ => accum2
            }
          }
        }
        val label = groupNames(maxIdx)
        accum :+ (label, maxIdx, maxScore)
      }
    }

  }

}
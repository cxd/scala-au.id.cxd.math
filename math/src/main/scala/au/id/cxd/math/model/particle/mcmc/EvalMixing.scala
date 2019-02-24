package au.id.cxd.math.model.particle.mcmc

import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.probability.analysis.Manova
import breeze.linalg.{DenseMatrix, sum}

/**
  * evaluate the mixing of variance between and within runs.
  *
  * Each run must be of equal length.
  *
  * @param runLabels
  * @param runs
  */
class EvalMixing(val runs:Seq[(String,Seq[Double])]) {

  /**
    * convert the run sequences to a set of labels for each run and a matrix
    * @return
    */
  def convertToMatrix(): (List[String], DenseMatrix[Double]) = {
    val rundata = runs.map { _._2 }
    val len = rundata(0).length
    val m = rundata.length*2
    val n = len/2
    val halvedruns = runs.foldLeft(Seq[Seq[Double]]()) {
      (accum, run) => {
        val slice1 = for(i <- 0 until n) yield i
        val slice2 = for(i <- n until len) yield i
        val run1 = Seq[Double]() ++ slice1.map { i => run._2(i) }
        val run2 = Seq[Double]() ++ slice2.map { i => run._2(i) }
        val a1 = accum :+ run1
        val a2 = a1 :+ run2
        a2
      }
    }
    val groupLabels = runs.foldLeft(List[String]()) {
      (accum, run) => (accum :+ run._1) :+ run._1
    }
    val mat = DenseMatrix.tabulate[Double](m, n) {
      case (i,j) => {
        val run = halvedruns(i)
        run(j)
      }
    }
    (groupLabels, mat)
  }

  /**
    * compute the between group variation
    *
    * m - groups
    *
    * n - number of samples per group
    *
    * n/(m-1) \sum_{j=1} (\bar{\psi}_{.,j} - \bar{\psi}_{..})^2
    *
    * see p 284 Gelman
    *
    * @param manova
    * @param mat
    * @return
    */
  def computeB (manova:Manova, mat:DenseMatrix[Double]): (Double, DenseMatrix[Double], DenseMatrix[Double]) = {
    // compute the group means
    val (idx, g_mu) = manova.computeGroupMeans(mat)
    // compute the total means
    val mu = ColMeans(mat)
    // m = number of chains or groups
    // n = length of each chain.
    val n = mat.cols
    val m = manova.groups.size
    val c = n / (m - 1.0)
    // sum of squares
    val ss = DenseMatrix.tabulate(g_mu.rows, g_mu.cols) {
      case (i,j) => Math.pow(g_mu(i,j) - mu(0,j),2.0)
    }

    (c * sum(ss), g_mu, mu)
  }

  /**
    * compute within group variation.
    * @param manova
    * @param mat
    */
  def computeW (manova:Manova, mat:DenseMatrix[Double]): (Double, DenseMatrix[Double]) = {
    val (idx, g_var) = manova.computeGroupVariance(mat)
    val m = manova.groups.size
    val w = 1/m.toDouble * sum(g_var)
    (w, g_var)
  }

  // TODO: work on calculation of R hat for indicator of mixing.
  //
  /**
    *
    * compute R hat
    *
    * R hat should approach 1 as n approaches infinity if the model is well mixed.
    * @param runLabels
    * @param mat
    * @return
    */
  def computeR(runLabels:List[String], mat:DenseMatrix[Double]): Double = {
    val manova = new Manova(groupNames=runLabels, data=mat)
    // total variation
    val (cov,t) = manova.computeT(mat)
    val T = t
    // between variation
    val (b,g_mu, mu) = computeB(manova, mat)
    val B = b
    // within sample variation
    val (w,g_var) = computeW(manova, mat)
    val W = w

    val n = mat.rows
    val m = mat.cols
    // variance estimate
    val c1 = (n - 1.0)/n
    val c2 = 1.0/n
    val V = c1*W + c2*B
    // W^{-1}V
    val Rhat = Math.sqrt(V)
    Rhat
  }

  def op(): Double = {
    val (labels, mat) = convertToMatrix()
    val rHat = computeR(labels, mat)
    rHat
  }

}
object EvalMixing {
  def apply(runs:Seq[(String,Seq[Double])]): Double = {
    val mix = new EvalMixing(runs)
    mix.op()
  }
}
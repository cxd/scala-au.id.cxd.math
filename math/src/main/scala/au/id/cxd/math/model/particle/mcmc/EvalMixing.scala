package au.id.cxd.math.model.particle.mcmc

import au.id.cxd.math.probability.analysis.Manova
import breeze.linalg.DenseMatrix

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
  def computeR(runLabels:List[String], mat:DenseMatrix[Double]): DenseMatrix[Double] = {
    val manova = new Manova(groupNames=runLabels, data=mat)
    // total variation
    val (cov,t) = manova.computeT(mat)
    val T = t
    // between variation
    val (b,mu) = manova.computeB(mat)
    val B = b
    // within sample variation
    val W = T + (-1.0 * B)

    val n = mat.rows
    val m = mat.cols
    // variance estimate
    val c1 = (n - 1.0)/n
    val c2 = 1.0/n
    val V = c1*W + c2*B
    val Rhat = (V \ W).map(x => Math.sqrt(x))
    Rhat
  }

  def op(): DenseMatrix[Double] = {
    val (labels, mat) = convertToMatrix()
    val rHat = computeR(labels, mat)
    rHat
  }

}
object EvalMixing {
  def apply(runs:Seq[(String,Seq[Double])]): DenseMatrix[Double] = {
    val mix = new EvalMixing(runs)
    mix.op()
  }
}
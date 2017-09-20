package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.Cov
import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.probability.continuous.FDistribution
import breeze.linalg.{DenseMatrix, DenseVector, det, eigSym, inv}

import scala.collection.immutable.Stream

/** ##import MathJax
  *
  * Manova for analysis of variance testing
  * whether there is a difference in variation between groups.
  *
  * In order to perform the analysis several related methods are available.
  *
  * - Wilks Lambda
  * - Roy's Largest Root
  * - Pillai's Trace
  * - Lawes-Hotelling trace
  *
  * Each of the statistics rely on the following matrices.
  *
  * # Total Variation
  * $$
  * T = Cov(X)*(n-1)
  * $$
  * The matrix $T$ is $k \times k$ in size where $k$ is the number of attributes in $X$.
  * $n$ is the total number of rows in the data set.
  *
  * # Within Group Variation
  * $$
  * W = T - B
  * $$
  * where B is the between group variation.
  *
  * # Between Group Variation
  *
  * In order to compute between group variation we need a column or vector in the data
  * that is a group categorical variable.
  *
  * From this value we then determine the set of group means $G_{\mu}$
  * which for m groups are m group means vectors. We then compute
  *
  * $$
  * B = n G_{\mu}' G_{\mu}
  * $$
  *
  * $n$ here is the column vector with the number of observations for each group.
  *
  * Note that $G_{\mu}$ is a matrix of $m \times k$ where $k$ is the number of attributes
  * and $m$ the number of groups. The matrix $B$ is $k \times k$.
  *
  * # Statistics
  *
  * ## Wilks Lambda
  *
  * Given the design matrix compute wilks lambda
  *
  * $$
  * \Lambda = \frac{|W|}{|T|} = \prod_{i=1}^k \frac{1}{1+\lambda_i}
  * $$
  * Where $W$ is the within sample variation and $T$ is the total variation.
  *
  * ## Roy's Largest Root
  *
  * $$
  * \lambda_{max}
  * $$
  *
  * ## Pillai's Trace
  *
  * $$
  * V = \sum_{i=1}^k \frac{\lambda_i}{1+\lambda_i}
  * $$
  *
  * ## Lawes Hotelling trace
  *
  * U = \sum_{i=1}^k \lambda+i
  * */
class Manova(method:ManovaMethod, groupNames: List[String], data: DenseMatrix[Double], val alpha:Double = 0.05) {

  lazy val groups = groupIndexes(groupNames)

  lazy val partitions: List[(String, DenseMatrix[Double])] = partitionGroups(data)(groups)

  /**
    * identify the group indexes.
    *
    * @param groupNames
    */
  def groupIndexes(groupNames: List[String]) =
    groupNames.foldLeft((0, Map[String, Vector[Int]]())) {
      (accum, groupName) => {
        val i = accum._1
        val groups = accum._2
        (i + 1, groups.contains(groupName.toLowerCase) match {
          case true => {
            val indexes = groups(groupName.toLowerCase) :+ i
            (groups - (groupName.toLowerCase)) + (groupName.toLowerCase -> indexes)
          }
          case _ => (groups + (groupName.toLowerCase -> Vector(i)))
        })
      }
    }._2

  /**
    * extract the partitions for each of the groups.
    *
    * @param m
    * @param groups
    * @return
    */
  def partitionGroups(m: DenseMatrix[Double])(groups: Map[String, Vector[Int]]): List[(String, DenseMatrix[Double])] = {
    groups.foldLeft(List[(String, DenseMatrix[Double])]()) {
      (accum, group) => {
        val key = group._1
        val idx = group._2
        val submat = m(idx, ::).toDenseMatrix
        accum :+ (key, submat)
      }
    }
  }

  /**
    * compute the total variation
    *
    * @param m
    * @return
    */
  def computeT(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val C = Cov(m)
    val n = m.rows
    val T = C.map(_ * (n - 1))
    T
  }

  /**
    * compute the between group variation
    *
    * @param m
    */
  def computeB(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    // generate a matrix of column means for each group.
    val mu = partitions.foldLeft((0, DenseMatrix.zeros[Double](partitions.length, m.cols))) {
      (accum, partition) => {
        val key = partition._1
        val submat = partition._2
        val mu = ColMeans(submat)
        val mat = accum._2
        mat(accum._1, ::) := mu(0, ::)
        (accum._1 + 1, mat)
      }
    }
    val counts = DenseMatrix.tabulate[Double](partitions.length, 1) {
      case (i, j) => partitions(i)._2.rows
    }
    // TODO: debug the dimensions for group sizes and overall counts for each group.
    val temp = (mu._2.t * counts)
    val B = temp * mu._2
    B
  }

  def eig(m: DenseMatrix[Double]) =
    eigSym(m)


  /**
    * calculate wilksLambda
    *
    * @param W
    * @param T
    * @param n
    * @param m
    * @param p
    * @return
    */
  def wilksLambda(eigenValues: DenseVector[Double], n: Int, m: Int, p: Int) = {

    val wilks = eigenValues.map { lambda => 1 / (1 + lambda) }.reduce(_ * _)

    val w = n - 1 - (p + m) / 2
    val df1 = p * (m - 1)
    val t = (Math.pow(df1,2) - 4) / Math.sqrt(Math.pow(p,2) + Math.pow(m-1,2) - 5)
    val df2 = (w*t * -df1 / 2 + 1).toInt
    val F = Math.pow(1-wilks, 1/t)/Math.pow(wilks, 1/t) * (df2/df1)
    ManovaStat("Wilk's Lambda", wilks, df1, df2, F)
  }

  def roysLargestRoot(eigenValues: DenseVector[Double], n: Int, m: Int, p: Int) = {
    val maxLambda = eigenValues.toArray.max
    val df1 = p
    val d = Array(p, m-1).max
    val df2 = n - m - p - 1
    val F = (df2/df1)*maxLambda
    ManovaStat("Roys Largest Root", maxLambda, df1, df2, F)
  }

  def pillaisTrace(eigenValues:DenseVector[Double], n:Int, m:Int, p:Int) = {
    val total = eigenValues.map { lambda => lambda / (1 + lambda ) }.toArray.sum
    val d = Array(p,m-1).max
    val s = Array(p,m-1).min
    val df1 = s*d
    val df2 = s*(n - m - p + s)
    val F = (n-m-p+s)*total / (d*(s-total))
    ManovaStat("Pillai's Trace", total, df1, df2, F)
  }

  def lawesHotellingTrace(eigenValues:DenseVector[Double], n:Int, m:Int, p:Int) = {
    val U = eigenValues.toArray.sum
    val d = Array(p,m-1).max
    val s = Array(p,m-1).min
    val A = (Math.abs(m - p - 1) - 1)/2
    val B = (n - m - p - 1)/2
    val df1 = s*(2*A + s + 1)
    val df2 = 2*(s*B+1)
    val F = df2*U/(s*df1)
    ManovaStat("Lawes Hotelling Trace", U, df1, df2, F)
  }

  def selectMethod() = method match {
    case WilksLambda() => wilksLambda(_,_,_,_)
    case RoysLargestRoot() => roysLargestRoot(_,_,_,_)
    case PillaisTrace() => pillaisTrace(_,_,_,_)
    case LawesHotellingTrace() => lawesHotellingTrace(_,_,_,_)
  }

  def op() = {
    val n = data.rows
    val m = groups.size
    val p = data.cols
    val T = computeT(data)
    val B = computeB(data)
    val W = T + (-1.0 * B)
    val WinvB = inv(W) * B
    val eigenDecomp = eig(WinvB)
    val lambda = eigenDecomp.eigenvalues
    // call appropriate manova method. parameterise type of manova in use
    val inferenceFn = selectMethod()
    val manovaStat = inferenceFn(lambda, n, m, p)

    // perform inferential test of F statistic.
    def sequence(last: Double): Stream[Double] = {
      last #:: sequence(last + 0.1)
    }

    val fdist = FDistribution(manovaStat.df1, manovaStat.df2)
    val criticalVal = CriticalValue(fdist, UpperTail()) _

    // calculate the critical value F statistic for (k-1), (n-k) df at alpha level
    val critical = criticalVal(sequence(0.0).take(100))
    val test = critical.value(alpha)
    val reject = manovaStat.stat > test
    // upper tail
    // get the probability of the observed F value
    val prob = fdist.pdf(manovaStat.stat)
    // calculating the minimum alpha-value for the p Value see Wackerly section 10.6
    // the pvalue in the case of the F-Distribution is equal to P(w_0 >= observedStat)
    // P(w_0 >= W) = 1 - P(w_0 < W)
    val pValue = 1 - fdist.integral(0.0, manovaStat.stat)

    // TODO: we should format the outputs of manova in the anova table format, although this will differ based on
    // the selected method.
    // generally all methods will be used for comparison.

    ManovaTest(reject= reject,
      criticalVal = test,
      alpha = alpha,
      manovaStat = manovaStat)

  }


}


object Manova {

  /**
    * perform inference on the supply groups and data.
    * @param groupNames
    * @param data
    * @param alpha
    * @param method
    *               the default manova method is set to wilks lambda
    * @return
    */
  def apply(groupNames: List[String], data: DenseMatrix[Double], alpha:Double = 0.05, method:ManovaMethod = WilksLambda()) =
    new Manova(method,groupNames,data,alpha).op

  /**
    * apply the four available tests for differences in group variation
    * The methods applied are
    * - wilks lambda
    * - roys largest root
    * - pillais trace
    * - lawes hotelling trace
    * @param groupNames
    * @param data
    * @param alpha
    * @return
    */
  def applyAll(groupNames:List[String], data:DenseMatrix[Double], alpha:Double=0.05) =
    List(WilksLambda(),
      RoysLargestRoot(),
      PillaisTrace(),
      LawesHotellingTrace()).map (method => Manova(groupNames, data, alpha, method))

}

abstract class ManovaMethod() {}
case class WilksLambda() extends ManovaMethod() {}
case class RoysLargestRoot() extends ManovaMethod() {}
case class PillaisTrace() extends ManovaMethod() {}
case class LawesHotellingTrace() extends ManovaMethod() {}

/**
  * a class to hold the statistic/
  * @param name
  * @param stat
  * @param df1
  * @param df2
  * @param Fstatistic
  */
case class ManovaStat(val name:String, val stat: Double, val df1: Int, val df2: Int, val Fstatistic:Double) {

  override def toString () =
    s"""
       |Manova  method $name
       |Statistic: $stat
       |Df1: $df1
       |Df2: $df2
       |F-Statistic: $Fstatistic
     """.stripMargin

}

case class ManovaTest(val reject:Boolean, val alpha:Double, val criticalVal:Double, manovaStat:ManovaStat) {
  override def toString() =
    s"""
       |Manova Test at alpha = $alpha
       |
       |Reject null hypothesis? $reject
       |Critical Value: $criticalVal
       |
       |${manovaStat.toString}
     """.stripMargin
}

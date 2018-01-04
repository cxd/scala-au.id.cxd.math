package au.id.cxd.math.probability.analysis

import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.function.distance.Cov
import au.id.cxd.math.probability.continuous.FDistribution
import breeze.linalg.{DenseMatrix, DenseVector, det, eigSym, inv, svd}

import scala.collection.immutable.Stream

/** ##import MathJax
  *
  * Manova for analysis of variance testing
  * whether there is a difference in variation between groups.
  *
  * The resulting f statistic is used to test the null hypothesis that the groups within the sample have
  * the same multivariate mean and common covariance, while the alternate hypothesis indicates
  * that the groups within the sample do not share the same mean and do not have common covariance.
  *
  * The procedure assumes multivariate normality (test for this prior to using manova) and common variance between groups.
  *
  * Given the above assumptions hold, it is useful to perform the manova procedure with standardised data.
  *
  * The eigen value solution used in the process may differ from other packages, possibly because there are multiple solutions
  * to the problem used in the calculation. The statistics obtained should be reported when displaying the output,
  * those should include, the critical value for the significance level, the calculated f-statistics, the degrees of freedom and the
  * calculated statistic corresponding to the method.
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
  * \Lambda = \frac{|W|}{|T|} = \prod_{i=1}&#94;k \frac{1}{1+\lambda_i}
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
  * V = \sum_{i=1}&#94;k \frac{\lambda_i}{1+\lambda_i}
  * $$
  *
  * ## Lawes Hotelling trace
  * $$
  * U = \sum_{i=1}&#94;k \lambda+i
  * $$
  *
  **/
class Manova(method: ManovaMethod, groupNames: List[String], data: DenseMatrix[Double], val alpha: Double = 0.05) {

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
    val keys = groups.keys.toIndexedSeq.sorted
    keys.foldLeft(List[(String, DenseMatrix[Double])]()) {
      (accum, key) => {
        val idx = groups(key)
        val submat = m(idx, ::).toDenseMatrix
        accum :+ (key, submat)
      }
    }
  }

  /**
    * compute the total variation
    *
    * # Total Variation
    * $$
    * T = Cov(X)*(n-1)
    * $$
    * The matrix $T$ is $k \times k$ in size where $k$ is the number of attributes in $X$.
    * $n$ is the total number of rows in the data set.
    *
    * @param m
    * @return
    */
  def computeT(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val C = Cov(m)
    val n = m.rows
    val T = C.map(_ * (n.toDouble - 1.0))
    T
  }

  /**
    * compute the between group variation
    *
    * # Between Group Variation
    *
    * In order to compute between group variation we need a column or vector in the data
    * that is a group categorical variable.
    *
    * From this value we then determine the set of group means $G_{\mu}$
    * which for m groups are m group means vectors. We then compute
    *
    *
    * $$
    * B = n G_{\mu}' G_{\mu}
    * $$
    *
    * $n$ here is the column vector with the number of observations for each group.
    *
    *
    * Note that $G_{\mu}$ is a matrix of $m \times k$ where $k$ is the number of attributes
    * and $m$ the number of groups.
    *
    * The matrix $B$ is $k \times k$.
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

    val nSS = DenseMatrix.tabulate[Double](partitions.length, m.cols) {
      case (i, j) => mu._2(i, j) * partitions(i)._2.rows
    }

    val B = nSS.t * mu._2
    B
  }

  /**
    * eigenvalues approximated by the square of the singular values
    * http://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm
    *
    * @param m
    * @return
    */
  def eig(m: DenseMatrix[Double]): DenseVector[Double] = {
    val svd.SVD(u, sigma, vt) = svd(m)
    DenseVector.tabulate[Double](sigma.length) {
      case i => Math.pow(sigma(i), 2.0)
    }
  }


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

    val wilks = eigenValues.map { lambda => 1.0 / (1.0 + lambda) }.reduce(_ * _)

    val w = n.toDouble - 1.0 - (p + m) / 2.0
    val df1 = p * (m - 1.0)
    val denom = (Math.pow(p, 2) + Math.pow(m - 1.0, 2) - 5.0)
    val t = if (df1 == 2 || denom == 0) {
      1.0
    } else {
      Math.sqrt((Math.pow(df1, 2) - 4d) / denom)
    }
    val df2 = (w * t - df1 / 2d + 1d)
    val F = ((1.0 - Math.pow(wilks, 1.0 / t)) / Math.pow(wilks, 1.0 / t)) * (df2 / df1)
    ManovaStat("Wilk's Lambda", wilks, df1.toInt, df2.toInt, F, eigenValues)
  }

  def roysLargestRoot(eigenValues: DenseVector[Double], n: Int, m: Int, p: Int) = {
    val maxLambda = eigenValues.toArray.max
    val d = Array(p, m - 1d).max
    val df1 = d
    val df2 = n - m - d - 1d
    val F = (df2 / df1) * maxLambda
    ManovaStat("Roys Largest Root", maxLambda, df1.toInt, df2.toInt, F, eigenValues)
  }

  def pillaisTrace(eigenValues: DenseVector[Double], n: Int, m: Int, p: Int) = {
    val total = eigenValues.map { lambda => lambda / (1 + lambda) }.toArray.sum
    val d = Array(p, m - 1d).max
    val s = Array(p, m - 1d).min
    val df1 = s * d
    val df2 = s * (n - m - p + s)
    val F = (n - m - p + s) * total / (d * (s - total))
    ManovaStat("Pillai's Trace", total, df1.toInt, df2.toInt, F, eigenValues)
  }

  def lawesHotellingTrace(eigenValues: DenseVector[Double], n: Int, m: Int, p: Int) = {
    val U = eigenValues.toArray.sum
    val d = Array(p, m - 1d).max
    val s = Array(p, m - 1d).min
    val A = (Math.abs(m - p - 1d) - 1d) / 2d
    val B = (n - m - p - 1d) / 2d
    val df1 = s * (2d * A + s + 1d)
    val df2 = 2d * (s * B + 1d)
    val F = df2 * U / (s * df1)
    ManovaStat("Lawes Hotelling Trace", U, df1.toInt, df2.toInt, F, eigenValues)
  }

  def selectMethod() = method match {
    case WilksLambda() => wilksLambda(_, _, _, _)
    case RoysLargestRoot() => roysLargestRoot(_, _, _, _)
    case PillaisTrace() => pillaisTrace(_, _, _, _)
    case LawesHotellingTrace() => lawesHotellingTrace(_, _, _, _)
  }

  def op() = {
    val n = data.rows
    val m = groups.size
    val p = data.cols
    val T = computeT(data)
    val B = computeB(data)
    val W = T + (-1.0 * B)
    // W^{-1}B
    val WinvB = W \ B
    val lambda = eig(WinvB)
    // call appropriate manova method. parameterise type of manova in use
    val inferenceFn = selectMethod()
    val manovaStat = inferenceFn(lambda, n, m, p)

    // TODO: debug the calculation of the critical value with high degree of freedom
    // in F-distribution.
    //println(s"DF1 ${manovaStat.df1} DF2 ${manovaStat.df2}")

    val fdist = FDistribution(manovaStat.df1, manovaStat.df2)

    // calculate the critical value F statistic for (k-1), (n-k) df at alpha level
    val test = fdist.invcdf(1.0 - alpha)
    /**
      * test H_0: groups chare the same mean and common variance (ie share the same MVN distribution)
      * vs H_1: at least one group does not share the same mean and common variance.
      */
    val reject = manovaStat.Fstatistic > test
    // upper tail
    // get the probability of the observed F value
    val prob = fdist.pdf(manovaStat.Fstatistic)
    // calculating the minimum alpha-value for the p Value see Wackerly section 10.6
    // the pvalue in the case of the F-Distribution is equal to P(w_0 >= observedStat)
    // P(w_0 >= W) = 1 - P(w_0 < W)
    val pValue = fdist.cdf(manovaStat.Fstatistic)

    ManovaTest(reject = reject,
      criticalVal = test,
      alpha = alpha,
      manovaStat = manovaStat,
      pValue = pValue)

  }


}


object Manova {


  /**
    * create an instance of manova but do not perform the operation.
    *
    * @param groupNames
    * @param data
    * @param alpha
    * @param method
    * @return
    */
  def build(groupNames: List[String], data: DenseMatrix[Double], alpha: Double = 0.05, method: ManovaMethod = WilksLambda()) =
    new Manova(method, groupNames, data, alpha)

  /**
    * perform inference on the supply groups and data.
    *
    * @param groupNames
    * @param data
    * @param alpha
    * @param method
    * the default manova method is set to wilks lambda
    * @return
    */
  def apply(groupNames: List[String], data: DenseMatrix[Double], alpha: Double = 0.05, method: ManovaMethod = WilksLambda()) =
    new Manova(method, groupNames, data, alpha).op

  /**
    * apply the four available tests for differences in group variation
    * The methods applied are
    * - wilks lambda
    * - roys largest root
    * - pillais trace
    * - lawes hotelling trace
    *
    * @param groupNames
    * @param data
    * @param alpha
    * @return
    */
  def applyAll(groupNames: List[String], data: DenseMatrix[Double], alpha: Double = 0.05) =
    List(WilksLambda(),
      RoysLargestRoot(),
      PillaisTrace(),
      LawesHotellingTrace()).map(method => Manova(groupNames, data, alpha, method))

}

abstract class ManovaMethod() {}

case class WilksLambda() extends ManovaMethod() {}

case class RoysLargestRoot() extends ManovaMethod() {}

case class PillaisTrace() extends ManovaMethod() {}

case class LawesHotellingTrace() extends ManovaMethod() {}

/**
  * a class to hold the statistic/
  *
  * @param name
  *             the name of the method used to calculate the statistic
  * @param stat
  *             the resulting statistic
  * @param df1
  *            the first degree of freedom for the f-distribution
  * @param df2
  *            the second degree of freedom for the f-distribution
  * @param Fstatistic
  *             the measure calculated by the method following the F distribution at df1 and df2 degrees of freedom.
  *             the value is dependent on the kind of method used.
  */
case class ManovaStat(val name: String, val stat: Double, val df1: Int, val df2: Int, val Fstatistic: Double, val eigenValues: DenseVector[Double]) {

  override def toString() =
    s"""
       |Manova  method $name
       |Statistic: $stat
       |Df1: $df1
       |Df2: $df2
       |F-Statistic: $Fstatistic
       |
     """.stripMargin

}

/**
  * the outcome of the manova test
  * In the case of the MANOVA test the null hypothesis asserts that the groups share the same mean and common variance.
  * The f statistic is used to test whether the null hypothesis holds.
  *
  * @param reject
  *               a flag indicating whether to reject $H_0$
  * @param alpha
  *              the threshold where the alpha level was evaluated eg .05
  * @param criticalVal
  *               the critical value is the quantile of the fdistribution for the alpha threshold
  *               and the degrees of freedom produced in the test.
  *               In order to reject the null hypothesis the observed statistic must be greater than the
  *               critical value
  * @param pValue
  *               the pValue is the probability of the observed statistic for the
  *               f-distribution based on the resulting degrees of freedom
  * @param manovaStat
  *               the manova stat instance containing the parameters output from the test process.
  */
case class ManovaTest(val reject: Boolean, val alpha: Double, val criticalVal: Double, val pValue:Double, val manovaStat: ManovaStat) {

  def conclusion() = reject match {
    case true => "Sample groups do not share the same mean and common variance."
    case _ => "Sample groups are normally distributed with same mean and share common variance."
  }

  override def toString() =
    s"""
       |Manova Test at alpha = $alpha
       |
       |Reject null hypothesis? $reject
       |Critical Value: $criticalVal
       |Pr(>F): $pValue
       |
       |Test Assertion: ${manovaStat.Fstatistic} > $criticalVal is ${reject}
       |Conclusion: ${conclusion}
       |
       |${manovaStat.toString}
     """.stripMargin
}

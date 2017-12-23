package inference

import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis._
import breeze.linalg.{DenseMatrix, eigSym, inv, svd}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class TestManovaInference extends FlatSpec
  with Matchers {

  "Manova" should "partition groups" in new TestManovaData {
    val (groups, data) = read()

    val m = Manova.build(groups, data)

    val idx = m.groupIndexes(groups)

    val p = m.partitions

    // there are 5 groups in the test data.
    println(s"Group Length: ${idx.size}")
    println(s"Partitions: ${p.length}")

    p.length should be(expectedGroups)

    p.length should not be (0)
  }

  "Manova" should "compute total variation" in new TestManovaData {

    val (groups, data) = read()

    val m = Manova.build(groups, data)

    val T = m.computeT(data)

    val groupCount = groups.length

    println(s"T: (${T.rows} x ${T.cols})")

    T.rows should be (data.cols)
    T.cols should be (data.cols)
  }

  "Manova" should "compute between group variation" in new TestManovaData {
    val (groups, data) = read()

    val m = Manova.build(groups, data)
    val B = m.computeB(data)

    // we should have 20 groups in the data, hence 20 rows.


    println(s"B: (${B.rows} x ${B.cols})")

    println(B)

    B.rows should be (data.cols)
    B.cols should be (data.cols)
  }

  "Manova" should "compute inverse" in new TestManovaData {
    val (groups, data) = read()

    val m = Manova.build(groups, data)
    val B = m.computeB(data)

    val T = m.computeT(data)

    val W = T + (-1.0 * B)
    val Winv = inv(W)
    val WinvB = inv(W) * B

    println("Inverse W")
    println(s"Dim (${Winv.rows} x ${Winv.cols})")
    println(Winv)

    val svd.SVD(u, sigma, vt) = svd(Winv)


    println("WINV B")
    println(sigma)

    //val eigenDecomp = eig(WinvB)
  }

  /**
    * The expected output should correspond with the R method.
    *
    *
sROC 0.1-2 loaded
                 Df     Wilks approx F num Df den Df    Pr(>F)
as.factor(Group)  4 0.0021936   27.666     36 241.57 < 2.2e-16 ***
Residuals        72



    */
  "Manova" should "test wilks lambda" in new TestManovaData {

    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=WilksLambda())

    println(result)

    /*
    (ev2 <- s2$Eigenvalues)
# approximate wilks statistic
(wtest <- prod ( sapply(ev2, function(e) 1 / (1+e) ) ) )

                         [,1]     [,2]     [,3]      [,4]          [,5]         [,6]
as.factor(Group) 16.34763 6.514577 2.160207 0.1065971 -5.799145e-15 3.082132e-15
                          [,7]          [,8]         [,9]
as.factor(Group) -1.626989e-15 -5.293898e-16 2.084505e-17
[1] 0.002193564
     */

    println ("Eigenvalues")
    println(result.manovaStat.eigenValues)
    result.manovaStat.df1 should equal(36)
    result.manovaStat.df2 should equal(241)


    result.reject should be(true)
  }

  /**
    *                  Df    Roy approx F num Df den Df    Pr(>F)
as.factor(Group)  4 16.348    121.7      9     67 < 2.2e-16 ***
Residuals        72
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    */
  "Manova" should "test Roys Largest Root" in new TestManovaData {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=RoysLargestRoot())

    println(result)


    result.reject should be(true)
  }

  /**
    *                Df Pillai approx F num Df den Df    Pr(>F)
as.factor(Group)  4 2.5892   13.662     36    268 < 2.2e-16 ***
Residuals        72
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    */
  "Manova" should "test Pillais trace" in new TestManovaData {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=PillaisTrace())
    println(result)

    result.reject should be(true)
  }

  /**
    *                  Df Hotelling-Lawley approx F num Df den Df    Pr(>F)
as.factor(Group)  4           25.129   43.627     36    250 < 2.2e-16 ***
Residuals        72
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    */
  "Manova" should "test LawesHotelling trace" in new TestManovaData {
    val (groups, data) = read()

    val result = Manova(groups, data, alpha=0.05, method=LawesHotellingTrace())
    println(result)

    result.reject should be(true)
  }

}


trait TestManovaData extends MatrixReader {

  val fileName="test_mandible_data.csv"

  val expectedGroups :Int = 5

  override val skipHeader = true

  def read():(List[String], DenseMatrix[Double]) = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)
    // we know the headers on the first line
    // they are:
    // "Case","Group","X1","X2","X3","X4","X5","X6","X7","X8","X9","Sex"
    // we want to keep columns 2 .. 10 and discard the other three columns including the last.
    val m2 = mat(::, 2 to 10).toDenseMatrix
    // the data set is standardised prior to the procedure
    val X = StandardisedNormalisation().transform(m2)
    // we also know ahead of time that there are 5 groups in the data.
    val groups = mat(::,1).toArray.map(_.toString).toList
    (groups, X)
  }
}

package inference

import au.id.cxd.math.probability.analysis.Anova
import breeze.linalg.DenseMatrix
import org.scalatest._

/**
 * A test class for anova analysis.
 *
 * The example is taken from Example 13.2:
 *
 * Wackerly et al, "Mathematical Statistics with Applications 7th Edition" Brooks/Cole 2008, pp670,671
 *
 * our groups of students were subjected to different teaching techniques and tested at the end of a specified period of time.
 * As a result of dropouts from the experimental groups (due to sickness, transfer, etc.), the number of students varied from group to group.
 * Do the data shown in Table 13.2 present sufficient evidence to indicate a difference in mean achievement for the four teaching techniques?
 *
 * Created by cd on 15/11/14.
 */
class TestAnovaInference extends FlatSpec with ShouldMatchers {

  /**
   * columns correspond to k samples
   * rows correspond to sample observation Y_ij
   */
  val table = DenseMatrix(
    (65.0, 75.0, 59.0, 94.0),
    (87.0, 69.0, 78.0, 89.0),
    (73.0, 83.0, 67.0, 80.0),
    (79.0, 81.0, 62.0, 88.0),
    (81.0, 72.0, 83.0, 0.0),
    (69.0, 79.0, 76.0, 0.0),
    (0.0, 90.0, 0.0, 0.0))


  /**
   * the equvalent test in R  file testanova.R
   * the output we are looking for is:
   * testanova(X)
[1] "Total SS 26480.9642857143"
[1] "SST 2876.10714285716"
[1] "SSE 23604.8571428571"
[1] "MST 958.702380952387"
[1] "MSE 983.535714285714"
[1] "F-stat 0.974750959245682"
[1] "Critical Value:  3.00878657044736"
[1] "P-Value:  0.0497996204738284"
   */
  "Anova " should "reject null hypothesis" in {
    val anova = Anova(table)
    val testResult = anova.test(0.05)
    println(testResult)

    /**
      * The test result in R rests if the observed value of F = MST/MSE > F_alpha at the critical value.
      * It should reject the null hypothesis if F > F_alpha
      * and should not reject it otherwise.
      * In this case F-stat is not > Critical value hence should not be rejected.
      */
    testResult.reject should be(false)
  }

  /**
  > cm(X)
[1] 113030
   */
  "Anova " should "calculate TotalSS" in {
    val anova = Anova(table)
    val result = anova.cm()
    println("CM: " + result.cm)
    (result.cm > 113030.5) should not be(true)
    val result2 = anova.totalSS(result)
    println ("TotalSS: " + result2.totalSS)
  }

}

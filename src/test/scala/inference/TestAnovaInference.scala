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
[1] "Total SS 139447.464285714"
[1] "SST 19866.6071428571"
[1] "SSE 119580.857142857"
[1] "MST 6622.20238095238"
[1] "MSE 4982.53571428571"
[1] "F-stat 1.32908277244566"
   */
  "Anova " should "reject null hypothesis" in {
    val anova = Anova(table)
    val testResult = anova.test(0.05)
    println(testResult)
    testResult.reject should be(true)
  }

}

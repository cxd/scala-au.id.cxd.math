package au.id.cxd.math.probability.analysis

/**
 *
 * A statistical test trait that returns a test result
 *
 * It represents the testing of a value within a given reject region
 * for a given type of distribution, or method of testing.
 *
 * Created by cd on 1/11/14.
 */
trait StatisticalTest {

  def test(alpha:Double) : TestResult

}

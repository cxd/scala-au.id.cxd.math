package au.id.cxd.math.probability.analysis

/**
 * A test result after performing a test on a hypothesis.
 *
 * Created by cd on 1/11/14.
 */
class TestResult(
                  /**
                   * significance
                   * test at the level of significance
                   * alpha
                   */
                  val significance:Double,
                  /**
                   * determine whether $H_0$ can be rejected
                   */
                  val reject:Boolean,
                  /**
                   * The p-value for the observed statistic
                   */
                  val pValue:Double,

                  /**
                   * the observed value
                   */
                  val observedValue:Double,
                  /**
                   * the critical value for the observed statistic.
                   */
                  val criticalValue:Double
) {

}
object TestResult {
  def apply(significance:Double,
             reject:Boolean,
             pValue:Double,
             observedValue:Double,
             criticalValue:Double) = new TestResult(significance, reject, pValue, observedValue, criticalValue)
}

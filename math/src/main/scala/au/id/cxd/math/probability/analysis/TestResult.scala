package au.id.cxd.math.probability.analysis

import au.id.cxd.math.probability.Distribution

import scala.collection.immutable.Stream

/**
  * A test result after performing a test on a hypothesis.
  *
  * Created by cd on 1/11/14.
  */
class TestResult(
                  /**
                    * name of the test.
                    */
                  val name: String,

                  /**
                    * significance
                    * test at the level of significance
                    * alpha
                    */
                  val significance: Double,

                  /**
                    * determine whether $H_0$ can be rejected
                    */
                  val reject: Boolean,

                  /**
                    * The p-value for the observed statistic
                    */
                  val pValue: Double,

                  /**
                    * the observed value
                    */
                  val observedValue: Double,

                  /**
                    * the critical value for the observed statistic.
                    */
                  val criticalValue: Double,

                  /**
                    * any messages associated with the output of the test.
                    */
                  val message: String = ""
                ) {

  override def toString: String =
    s"""
       |Test Results: $name
       |
       |significance = $significance
       |reject = $reject
       |pValue = $pValue
       |observedValue = $observedValue
       |criticalValue = $criticalValue
       |message = $message
     """.stripMargin

}

object TestResult {
  def apply(name: String,
            significance: Double,
            reject: Boolean,
            pValue: Double,
            observedValue: Double,
            criticalValue: Double,
            message: String = "") = new TestResult(name, significance, reject, pValue, observedValue, criticalValue, message)


}

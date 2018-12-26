package au.id.cxd.math.probability.analysis

/**
  * Shapiro Wilks
  * Univariate test for normality
  *
  * TODO:
  */
class ShapiroWilks(val data:Seq[Double], val alpha:Double=0.05) extends StatisticalTest {


  /**
    * the ordered function.
    * @return
    */
  def ordered(): Seq[Double] = data.sorted

  def indexed(set:Seq[Double]) = ???

  override def test(alpha: Double): TestResult = ???
}

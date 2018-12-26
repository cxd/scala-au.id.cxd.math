package au.id.cxd.math.probability.analysis

/**
  * Shapiro Wilks
  * Univariate test for normality
  *
  * TODO:
  *
  * see: https://ww2.mathworks.cn/matlabcentral/mlc-downloads/downloads/submissions/13964/versions/2/previews/swtest.m/index.html
  * for an example of implementation with references.
  *
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

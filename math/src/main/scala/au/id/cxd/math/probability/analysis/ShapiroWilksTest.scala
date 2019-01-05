package au.id.cxd.math.probability.analysis

/**
  * Shapiro Wilks
  * Univariate test for normality
  *
  * TODO:
  *
  * see:
  * https://ww2.mathworks.cn/matlabcentral/mlc-downloads/downloads/submissions/13964/versions/2/previews/swtest.m/index.html
  *
  * as well as:
  * https://github.com/wch/r-source/blob/trunk/src/library/stats/src/swilk.c
  *
  * Both reference the AS R94 article on the shapiro wilk normality test.
  * The R source version swilk.c is converted from the fortran implementation of that algorithm.
  * The matlab source being a DSL is much easier to read through.
  * However in both cases the coefficients of the polynomials are provided in tables and these are used in the expansion when
  * calculating the a_i coefficients in the statistic.
  *
  * In order to provide an implementation, the same constant coefficients will be used.
  * However simplification of the C version is attempted with heavy influence from the approach taken in matlab.
  *
  *
  */
class ShapiroWilksTest(val data:Seq[Double], val alpha:Double=0.05) extends StatisticalTest {


  /**
    * the ordered function.
    * @return
    */
  def ordered(): Seq[Double] = data.sorted

  def indexed(set:Seq[Double]) = ???

  override def test(alpha: Double): TestResult = ???
}

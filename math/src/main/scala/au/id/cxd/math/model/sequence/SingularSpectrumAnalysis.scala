package au.id.cxd.math.model.sequence

import au.id.cxd.math.function.series.WindowedMatrix
import au.id.cxd.math.model.components.PrincipleComponentsAnalysis
import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * An implementation of the singular spectrum analysis (SSA)
  *
  */
object SingularSpectrumAnalysis extends WindowedMatrix {

  /**
    * perform SSA on the supplied univariate series.
    * @param series
    * @param stride
    * @return
    *         returns tuple of
    *         eigenValues, eigenVectors, varExplained, projection
    *
    * At least two of the eigenVectors must have a trigonometric function offset by pi/2
    */
  def apply(series: Seq[Double], stride: Int): (DenseVector[Double], DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {
    val matrix = windowedMatrix(series, stride)
    val (eigenValues, eigenVectors, varExplained, projection) = PrincipleComponentsAnalysis(matrix)
    (eigenValues, eigenVectors, varExplained, projection)
  }

}

package au.id.cxd.math.function

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * ##import MathJax
  *
  * The cosine distance between two vectors calculated as
  *
  * $$
  *   cosdist = \frac{<x,y>}{<x,x><y,y>}
  * $$
  * where $<x,y>$ is the dot product between two vectors.
  *
  * Created by cd on 10/1/17.
  */
class CosineDistance {

  def op(x:DenseVector[Double], y:DenseVector[Double]) : Double = {
    val a = x.dot(y)
    val b = Math.sqrt(x.dot(x))
    val c = Math.sqrt(y.dot(y))
    a  / (b*c)
  }

  /**
    * for every row in the matrix X calculate the cosine distance from the vector y
    * @param X
    * @param y
    * @return
    */
  def op(X:DenseMatrix[Double], y:DenseVector[Double]): DenseVector[Double] = {
    val seq = for (i <- 0 until X.rows) yield (op(X(i,::).inner, y))
    DenseVector[Double](seq.toArray)
  }

}

object CosineDistance {
  def apply(x:DenseVector[Double], y:DenseVector[Double] ) = new CosineDistance().op(x,y)

  def apply(M:DenseMatrix[Double], y:DenseVector[Double]) = new CosineDistance().op(M,y)
}



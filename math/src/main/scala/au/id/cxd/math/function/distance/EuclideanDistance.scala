package au.id.cxd.math.function.distance

import breeze.linalg.DenseVector

class EuclideanDistance {

  /**
    * squared euclidean distance between two vectors.
    *
    * @param X
    * @param Y
    * @return
    */
  def dist(X: DenseVector[Double], Y: DenseVector[Double]): Double = {
    val delta = (X - Y)
    val dist = delta.t * delta
    dist
  }
}

object EuclideanDistance {
  def apply(X:DenseVector[Double], Y:DenseVector[Double]):Double = {
    new EuclideanDistance().dist(X,Y)
  }
}

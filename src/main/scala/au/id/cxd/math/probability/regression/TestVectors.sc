import breeze.linalg._
import breeze.numerics.pow

import au.id.cxd.math.probability.regression._

val X = for { x <- 1 to 10 } yield x

val powers = for { i <- 0 to 10 } yield i


val rows = for {
  x <- X
  row <- for { i <- powers } yield x^i
} yield row


val T = DenseVector.ones[Double](4)

val T2 = DenseVector.ones[Double](4) * 0.5

val T3 = DenseVector((for {i <- 0 to 3} yield Math.pow(2.0, i)).toArray)

val P = pow(T - T2, 2.0)
val total = sum(P)
println(total)
val result = total.asInstanceOf[Double] * 0.5
val M = DenseMatrix.ones[Double](4,4)
M(1,::) := T2.t
M(2,::):= T3.asInstanceOf[DenseVector[Double]].t
M
def testY(X:DenseVector[Double]) = {
  X.map(x => Math.sin(x))
}
val X1 = Array(0.0, Math.PI/2.0, Math.PI, Math.PI*2.0)
val X2 = DenseVector(X1)
val Y = testY(X2)
val ols = OrdLeastSquares(X2, Y, 3, 0.5)
val T1 = ols.train()
val error = T1._2
val A = DenseMatrix((1.0, 1.0, 1.0)).t

val B = DenseMatrix((1.5, 2.0),
  (1.5, 2.0),
  (1.5, 2.0))
val C = DenseMatrix.horzcat(A, B)
C.rows

val test = normalize(M(*,::))

val test2 = M - test


val X3 = DenseMatrix((1,1,1),(2,2,2))
val C1 = X3.t * X3
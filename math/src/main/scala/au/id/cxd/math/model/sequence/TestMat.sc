import breeze.linalg.{DenseVector, sum, *, DenseMatrix}

val test1 = DenseMatrix.tabulate(5, 5) {
  (i, j) => i + j
}
val total = sum(test1(*,::))

val total1 = (total / 2)
total1.asInstanceOf[DenseVector[Int]].size
test1(2, ::) := total1.t
val test2 = test1

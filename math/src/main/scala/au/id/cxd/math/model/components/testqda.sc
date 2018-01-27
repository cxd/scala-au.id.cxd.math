import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.QuadraticDiscriminant
import breeze.linalg.{DenseMatrix, DenseVector, diag, inv, tile}

trait MandibleClassData {
  val train = new TrainMandibleDataSet()
  val test = new TestMandibleDataSet()

  val (trainGroups, trainData) = train.read()
  val (testGroups, testData) = test.read()
}

class TrainMandibleDataSet extends ManovaDataSet {
  val fileName = "test_mandible_train.csv"
}

class TestMandibleDataSet extends ManovaDataSet {
  val fileName = "test_mandible_test.csv"
}


trait ManovaDataSet extends MatrixReader {

  val fileName:String

  val expectedGroups :Int = 5

  override val skipHeader = true

  def read():(List[String], DenseMatrix[Double]) = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)
    // we know the headers on the first line
    // they are:
    // "Case","Group","X1","X2","X3","X4","X5","X6","X7","X8","X9","Sex"
    // we want to keep columns 2 .. 10 and discard the other three columns including the last.
    val m2 = mat(::, 2 to 10).toDenseMatrix
    // the data set is standardised prior to the procedure
    val X = StandardisedNormalisation().transform(m2)
    // we also know ahead of time that there are 5 groups in the data.
    val groups = mat(::,1).toArray.map(_.toString).toList
    (groups, X)
  }

  def readNonStandardised():(List[String], DenseMatrix[Double]) = {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    val mat = read(file)
    // we know the headers on the first line
    // they are:
    // "Case","Group","X1","X2","X3","X4","X5","X6","X7","X8","X9","Sex"
    // we want to keep columns 2 .. 10 and discard the other three columns including the last.
    val m2 = mat(::, 2 to 10).toDenseMatrix
    // the data set is standardised prior to the procedure
    // we also know ahead of time that there are 5 groups in the data.
    val groups = mat(::,1).toArray.map(_.toString).toList
    (groups, m2)
  }
}


def dim(m:DenseMatrix[Double]) = println(m.rows, m.cols)
def dim(v:DenseVector[Double]) = println(v.length)

def isSymmetric(m:DenseMatrix[Double]) = {
  (for (i<-0 until m.rows) yield i)
    .zip(for (j <- 0 until m.cols) yield j).forall {
    pair => {
      val (i,j) = pair
      val a = m(i,j)
      val b = m(j,i)
      val delta = Math.abs(a-b)
      delta <= 0.5
    }
  }
}

val data1 = new MandibleClassData {}

val (groups, data) = (data1.trainGroups, data1.trainData)
val groupParams = QuadraticDiscriminant(data, groups)

val (testGroups, y1) = (data1.testGroups, data1.testData)


val (gname, gdata) = groupParams.head
val (mui, eV, eVecs, varExp, proj, wi0) = gdata
val D = DenseMatrix.tabulate[Double](eV.length, eV.length) {
  case (i, j) => if (i == j) eV(i)
  else 0.0
}
val Dinv = inv(D)

val y = y1(0,::)

val logDetSigma = eV.reduce(_ + _)

val delta = y.inner.mapPairs { case (i,v) => {
  v - mui(0,i)
}}
val x = eVecs.t * delta
val Wi1 = (x.t * Dinv * x)
/**
  * $$
  * x'\Sigma_i&#94;{-1} \mu_i
  * $$
  */
// note this is a 1-d vector of length 1
//val Wi2 = x.t * Dinv * m

//val Wi = -0.5*Wi1 + Wi2(0)

//val gi =  Wi - 0.5*wi(0,0) - 0.5*logDetSigma + wi0

val gi = -0.5 * Wi1 - 0.5 * logDetSigma + wi0

gi
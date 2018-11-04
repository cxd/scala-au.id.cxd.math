package model.components

import java.io.File

import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.model.components.QuadraticDiscriminant
import breeze.linalg.DenseMatrix
import inference.TestManovaData
import org.scalatest.{FlatSpec, Matchers}

class TestQDA extends FlatSpec with Matchers {


  "QDA" should "compute quadratic parameters" in new TestManovaData {
    val (groups, data) = read()

    val groupParams = QuadraticDiscriminant(data, groups)

    println(groupParams.length)
    println(groups.distinct.length)
    groupParams.length should equal(groups.distinct.length)
  }

  "QDA" should "label with params" in new MandibleClassData {

    val groupParams = QuadraticDiscriminant(data, groups)

    val predictedClasses = QuadraticDiscriminant.classifyDiscriminant(data, groupParams)

    val predictGroups = predictedClasses.map(_._1)
    val crosstab = CrossTabulate[String](groups.toSeq, predictGroups.toSeq)

    println(crosstab)

    val metrics = CrossTabulate.metrics(crosstab)
    println(metrics)
  }


  "QDA" should "classify with params" in new MandibleClassData {

    val groupParams = QuadraticDiscriminant(trainData, trainGroups)

    val predictedClasses = QuadraticDiscriminant.classifyDiscriminant(testData, groupParams)

    val predictGroups = predictedClasses.map(_._1)
    val crosstab = CrossTabulate[String](testGroups.toSeq, predictGroups.toSeq)

    println(crosstab)

    val metrics = CrossTabulate.metrics(crosstab)
    println(metrics)
  }
}

trait MandibleClassData {
  val train = new TrainMandibleDataSet()
  val test = new TestMandibleDataSet()
  val all = new AllMandibleDataSet()

  val (groups, data) = all.read()
  val (trainGroups, trainData) = train.read()
  val (testGroups, testData) = test.read()
}

class TrainMandibleDataSet extends ManovaDataSet {
  val fileName = "test_mandible_train.csv"
}

class TestMandibleDataSet extends ManovaDataSet {
  val fileName = "test_mandible_test.csv"
}

class AllMandibleDataSet extends ManovaDataSet {
  val fileName = "test_mandible_data.csv"
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
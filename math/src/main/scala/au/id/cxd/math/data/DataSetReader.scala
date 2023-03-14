package au.id.cxd.math.data

import scala.util.Success
import java.io.File

import scala.concurrent.Future
import scala.concurrent.duration
import au.id.cxd.math.function.transform.{ContinuousTransform, IdentityTransform}

import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.concurrent.Await._

/**
  * a base class for reading data sets
  */
trait DataSetReader {
  import scala.concurrent.ExecutionContext.Implicits.global

  val continuousCols:List[Int]

  val discreteCols:List[Int]

  // using 60% for training, 20% for CV and test.
  val partitions:Array[Double] = Array[Double](0.6, 0.2, 0.2)

  val file:File

  val timeout:Duration = 10.seconds

  /**
    * override the continuous transformation function in order to perform standardisation.
    */
  val continuousFn:ContinuousTransform = new IdentityTransform()

  /**
    * read the data and generate the partitions.
    *
    * @return
    */
  def read():Future[DataSet] = {
    // apply the standardised normalisation to continuous columns
    val processor = PreProcessor(file, discreteCols, continuousCols, continuousFn)

    val dataSet = processor.read()
    dataSet map {
      data => {
        data.createPartitions(partitions(0), partitions(1), partitions(2))
        data
      }
    }
  }

  /**
    * read the data synchronously
    * @return
    */
  def readSync():DataSet = {
    val datasetF = read()
    result(datasetF, timeout)
  }

}

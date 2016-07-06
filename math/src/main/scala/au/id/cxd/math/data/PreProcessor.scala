package au.id.cxd.math.data

import java.io.File

import au.id.cxd.math.data.{CsvReader, MatrixReader}
import au.id.cxd.math.function.ContinuousTransform
import breeze.linalg.DenseMatrix

import scala.collection.mutable
import scala.concurrent.Future

/**
  * The preprocessor in this case will combine
  * continuous columns of data togethor with discrete columns of data.
  * Discrete columns will be converted into indicator matrices.
  *
  * Created by cd on 3/05/2016.
  */
class PreProcessor(val file: File, val discreteColumns: List[Int], val continuousColumns: List[Int],
                   val processContinuousFn: ContinuousTransform) {

  import scala.concurrent.ExecutionContext.Implicits.global

  /**
    * split the data into continuous and discrete partitions
    * based on the column input.
    *
    * @param data
    * @return
    */
  def splitColumns(data: mutable.Buffer[mutable.Buffer[String]]): (List[mutable.Buffer[String]], List[mutable.Buffer[String]]) =
    data.foldLeft(List[mutable.Buffer[String]](), List[mutable.Buffer[String]]())(splitRow)

  /**
    * split a single row
    * prepend the result into the accumulator
    *
    * @param accum
    * @param row
    * @return
    */
  def splitRow(accum: (List[mutable.Buffer[String]], List[mutable.Buffer[String]]), row: mutable.Buffer[String]) = {
    val idx = accum._1
    val discrete = discreteColumns.foldLeft(mutable.Buffer[String]()) {
      (collect, col) => {
        val item = row(col)
        collect.append(item)
        collect
      }
    }
    val continuous = continuousColumns.foldLeft(mutable.Buffer[String]()) {
      (collect, col) => {
        val item = row(col)
        collect.append(item)
        collect
      }
    }

    (discrete :: accum._1, continuous :: accum._2)
  }

  /**
    * read the csv data and produce the data set containing the
    * continuous data and the indicator variables as well as
    * the mappings from column name in original data to unique values of indicator variables
    *
    * @return
    */
  def read():Future[DataSet] = {
    val reader = CsvReader()
    // the read will reverse the lines
    val (discreteA, continuousA) = reader.readCsv(file, (List[mutable.Buffer[String]](), List[mutable.Buffer[String]]())) {
      (accum, line) => {
        continuousColumns.length > 0 && discreteColumns.length > 0 match {
          case true => splitRow(accum, line)
          case false => if (continuousColumns.length == 0) {
            (line :: accum._1, List[mutable.Buffer[String]]())
          } else {
            (List[mutable.Buffer[String]](), line :: accum._2)
          }
        }
        // TODO: work out how to include discreteMatrixAndKey and continuousMatrix internally
        // in this method to incrementally build the result.

      }
    }
    // reverse order O(n)
    val (discrete, continuous) = (discreteA.reverse, continuousA.reverse)



    // the discrete data needs to be transformed into an indicator matrix
    val discreteMatrixAndKey = Future {
      (discrete.length > 0) match {
        case true => {
          val discreteHeaders = discrete.head
          val discreteRows = discrete.tail
          DummyVariableBuilder.buildIndicatorMatrix(discreteHeaders, discreteRows)
        }
        case _ => None
      }
    }

    // the continuous matrix is not processed.
    val continuousMatrix = Future {
      (continuous.length > 0) match {
        case true => {
          val reader = new MatrixReader {}
          val mat = reader.convertToMatrix(continuous.length - 1, continuous.head.length, continuous)
          val processedMat = processContinuousFn.transform(mat)
          Some(processedMat)
        }
        case false =>
          None
      }
    }

    for {
      // calculate the discrete matrix and continuous matrix in parallel
      discreteResult <- discreteMatrixAndKey
      continuousResult <- continuousMatrix
      // combine the matrices if present with the continuous matrix first
      contCols = continuousResult match {
        case Some(mat) => mat.cols
        case _ => 0
      }

      discreteCols = discreteResult match {
        case Some(pair) => pair._1.cols
        case None => 0
      }

      // the continuous data will always be first
      dataSet = (continuousResult, discreteResult) match {
        case (Some(mat), Some(pair)) => {
          DenseMatrix.horzcat(mat, pair._1)
        }
        case (Some(mat), None) => mat
        case (None, Some(pair)) => pair._1
        case _ => DenseMatrix.zeros[Double](1, 1)
      }

      discreteMapping = discreteResult match {
        case Some(pair) => pair._2
        case _ => Map[String, Set[String]]()
      }
    } yield DataSet(dataSet, contCols, discreteCols, discreteMapping, processContinuousFn)


  }


}

object PreProcessor {

  def apply(file: File, discreteColumns: List[Int], continuousColumns: List[Int], transform: ContinuousTransform) =
    new PreProcessor(file, discreteColumns, continuousColumns, transform)
}

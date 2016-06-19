package au.id.cxd.math.data

import java.io.File

import au.id.cxd.math.data.{CsvReader, MatrixReader}
import au.id.cxd.math.function.ContinuousTransform
import breeze.linalg.DenseMatrix

import scala.collection.mutable

/**
  * The preprocessor in this case will combine
  * continuous columns of data togethor with discrete columns of data.
  * Discrete columns will be converted into indicator matrices.
  *
  * Created by cd on 3/05/2016.
  */
class PreProcessor(val file: File, val discreteColumns: List[Int], val continuousColumns: List[Int],
                   val processContinuousFn:ContinuousTransform ) {


  /**
    * split the data into continuous and discrete partitions
    * based on the column input.
    *
    * @param data
    * @return
    */
  def splitColumns(data: List[mutable.Buffer[String]]):(List[mutable.Buffer[String]], List[mutable.Buffer[String]]) =
    data.foldLeft(List[mutable.Buffer[String]](), List[mutable.Buffer[String]]()) {
      (accum, row) => {
        val idx = accum._1
        val discrete = discreteColumns.foldLeft(mutable.Buffer[String]()) {
          (collect, col) => {
            val item = row(col)
            collect :+ item
          }
        }
        val continuous = continuousColumns.foldLeft(mutable.Buffer[String]()) {
          (collect, col) => {
            val item = row(col)
            collect :+ item
          }
        }

        (accum._1 :+ discrete, accum._2 :+ continuous)
      }
    }

  /**
    * read the csv data and produce the data set containing the
    * continuous data and the indicator variables as well as
    * the mappings from column name in original data to unique values of indicator variables
    *
    * @return
    */
  def read(): DataSet = {
    val reader = CsvReader()
    val data = reader.readCsv(file)

    val (discrete, continuous) = continuousColumns.length > 0 && discreteColumns.length > 0 match {
      case true => splitColumns(data)
      case false => if (continuousColumns.length == 0) {
        (data, List[mutable.Buffer[String]]())
      } else {
        (List[mutable.Buffer[String]](), data)
      }
    }



    // the discrete data needs to be transformed into an indicator matrix
    val discreteMatrixAndKey = (discrete.length > 0) match {
      case true => {
        val discreteHeaders = discrete.head
        val discreteRows = discrete.tail
        val (mat, keyedMapping) = DummyVariableBuilder.buildIndicatorMatrix(discreteHeaders, discreteRows)
        Some(mat, keyedMapping)
      }
      case _ => None
    }

    // the continuous matrix is not processed.
    val continuousMatrix = (continuous.length > 0) match {
      case true => {
        val reader = new MatrixReader {}
        val mat = reader.convertToMatrix(continuous.length - 1, continuous.head.length, continuous)
        val processedMat = processContinuousFn.transform(mat)
        Some(processedMat)
      }
      case false => None
    }


    // combine the matrices if present with the continuous matrix first
    val contCols = continuousMatrix match {
      case Some(mat) => mat.cols
      case _ => 0
    }

    val discreteCols = discreteMatrixAndKey match {
      case Some(pair) => pair._1.cols
      case None => 0
    }

    // the continuous data will always be first
    val dataSet = (continuousMatrix, discreteMatrixAndKey) match {
      case (Some(mat), Some(pair)) => {
        DenseMatrix.horzcat(mat, pair._1)
      }
      case (Some(mat), None) => mat
      case (None, Some(pair)) => pair._1
      case _ => DenseMatrix.zeros[Double](1, 1)
    }

    val discreteMapping = discreteMatrixAndKey match {
      case Some(pair) => pair._2
      case _ => Map[String, Set[String]]()
    }

    DataSet(dataSet, contCols, discreteCols, discreteMapping, processContinuousFn)
  }


}

object PreProcessor {

  def apply(file: File, discreteColumns: List[Int], continuousColumns: List[Int], transform:ContinuousTransform) =
    new PreProcessor(file, discreteColumns, continuousColumns, transform)
}

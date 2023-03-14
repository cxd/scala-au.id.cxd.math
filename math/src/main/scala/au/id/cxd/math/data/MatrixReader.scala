package au.id.cxd.math.data

import java.io.File

import breeze.linalg.DenseMatrix

import scala.collection.mutable

/**
  * A simple convenience trait to read a matrix of doubles
  * from the csv data file.
  * Optionally override skipHeader to skip the first line
  * this defaults to true.
  * Created by cd on 1/05/2016.
  */
trait MatrixReader {
  /**
    * determine whether to skip the first line in the CSV file
    * which is taken to be a header line.
    *
    * @return
    */
  def skipHeader: Boolean = true

  private def toDouble(item: String): Double =
    if (item.equals("") || List("NA", "NULL", "None", "-", " ", "_", "*", "~").exists(n => item.equalsIgnoreCase(n))) 0.0
    else item.toDouble

  /**
    * convert the supplied data to a single row matrix with n cols
    *
    * @param cols
    * @param data
    * @return
    */
  def convertToRow(cols: Int, data: mutable.Buffer[String]): DenseMatrix[Double] =
    DenseMatrix.tabulate[Double](1, cols) {
      case (i, j) => toDouble (data(j))
    }

  /**
    * convert to a matrix
    * When reading data ensure that the data is within range of precision of Double.
    *
    * @param data
    * @return
    */
  def convertToMatrix(rows: Int, cols: Int, data: Seq[mutable.Buffer[String]]): DenseMatrix[Double] = {
    val M = DenseMatrix.zeros[Double](rows, cols)
    CsvReader().mapi[Seq[mutable.Buffer[String]], mutable.Buffer[String], String, DenseMatrix[Double]](data, M) {
      (pair, line) => {
        val rowIdx: Int = pair._1
        rowIdx == 0 && skipHeader match {
          case true => pair._2
          case _ => {
            val mat: DenseMatrix[Double] = pair._2
            val buffer = line.toBuffer
            val temp: DenseMatrix[Double] = DenseMatrix.tabulate[Double](1, buffer.length) {
              case (i, j) => toDouble (buffer(j))
            }
            val index = rowIdx - 1
            // matrix assignment requires range
            mat(index to index, ::) := temp
            mat
          }
        }
      }
    }
    M
  }

  /**
    * convert to matrix by filtering only selected columns from the CSV input.
    *
    * @param rows
    * @param cols
    * @param data
    * @return
    */
  def convertToMatrix(rows: Int, cols: Seq[Int], data: Seq[mutable.Buffer[String]]): DenseMatrix[Double] = {
    val M = DenseMatrix.zeros[Double](rows, cols.size)
    CsvReader().mapi[Seq[mutable.Buffer[String]], mutable.Buffer[String], String, DenseMatrix[Double]](data, M) {
      (pair, line) => {
        val rowIdx: Int = pair._1
        (rowIdx == 0 && skipHeader) match {
          case true => pair._2
          case _ =>
            val mat = pair._2
            val buffer = line
            val temp = buffer.foldLeft((0, List[Double]())) {
              (accum, item) =>
                val j = accum._1
                val data = accum._2
                val newData = cols.contains(j) match {
                  case true => data :+ toDouble (item)
                  case _ => data
                }
                (j + 1, newData)
            }
            val index = rowIdx - 1
            val tempTable = DenseMatrix.tabulate[Double](1, cols.size) { case (i, j) => temp._2(j) }
            mat(index to index, ::) := tempTable
            mat
        }
      }
    }
    M
  }

  /**
    * read the CSV file and return a matrix of rows x columns
    *
    * @param file
    * @return
    */
  def read(file: File): DenseMatrix[Double] = {
    val data = CsvReader().readCsv(file)
    val rows = skipHeader match {
      case true => data.length - 1
      case _ => data.length
    }
    val cols = data(0).length
    convertToMatrix(rows, cols, data.toSeq)
  }

  /**
    * read the CSV and convert the selected set of columns into a matrix
    *
    * @param file
    * @param cols
    * @return
    */
  def read(file: File, cols: Seq[Int]): DenseMatrix[Double] = {
    val data = CsvReader().readCsv(file)
    val rows = skipHeader match {
      case true => data.length - 1
      case _ => data.length
    }
    convertToMatrix(rows, cols, data.toSeq)
  }
}

object MatrixReader {

  /**
    * a helper class to read the file at a given path.
    *
    * @param path
    * @return
    */
  def readFileAt(path: String): DenseMatrix[Double] = {
    val reader = new MatrixReader {

    }
    val file = new File(path)
    reader.read(file)
  }

  /**
    * a helper class to read the file at a given path.
    *
    * @param path
    * @return
    */
  def readFileAt(path: String, cols: Seq[Int]): DenseMatrix[Double] = {
    val reader = new MatrixReader {

    }
    val file = new File(path)
    reader.read(file, cols)
  }
}

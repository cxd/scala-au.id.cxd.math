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
    * @return
    */
  def skipHeader:Boolean = true

  /**
    * convert the supplied data to a single row matrix with n cols
    * @param cols
    * @param data
    * @return
    */
  def convertToRow(cols:Int, data:mutable.Buffer[String]):DenseMatrix[Double] =
    DenseMatrix.tabulate[Double](1,cols) {
      case (i, j) => data(j) toDouble
    }

  /**
    * convert to a matrix
    * @param data
    * @return
    */
  def convertToMatrix(rows:Int, cols:Int, data:Seq[mutable.Buffer[String]]) = {
    val M = DenseMatrix.zeros[Double](rows, cols)
    CsvReader().mapi[Seq[mutable.Buffer[String]], mutable.Buffer[String], String, DenseMatrix[Double]](data, M) {
      (pair, line) => {
        val rowIdx:Int = pair._1
        rowIdx == 0 && skipHeader match {
          case true => pair._2
          case _ => {
            val mat:DenseMatrix[Double] = pair._2
            val buffer = line.toBuffer
            val temp:DenseMatrix[Double] = DenseMatrix.tabulate[Double](1,buffer.length) { case (i,j) => {
              buffer(j) match {
                case "" => 0.0
                case a => a.toDouble
              }
            } }
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
    * read the CSV file and return a matrix of rows x columns
    * @param file
    * @return
    */
  def read(file:File) = {
    val data = CsvReader().readCsv(file)
    val rows = skipHeader match {
      case true => data.length - 1
      case _ => data.length
    }
    val cols = data(0).length
    convertToMatrix(rows, cols, data)
  }
}

object MatrixReader {

  /**
    * a helper class to read the file at a given path.
    * @param path
    * @return
    */
  def readFileAt(path:String): DenseMatrix[Double] = {
    val reader = new MatrixReader {

    }
    val file = new File(path)
    reader.read(file)
  }
}

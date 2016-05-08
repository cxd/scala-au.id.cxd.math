package au.id.cxd.math.data

import java.io.File

import breeze.linalg.DenseMatrix

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
    * convert to a matrix
    * @param data
    * @return
    */
  def convertToMatrix(rows:Int, cols:Int, data:List[Array[String]]) = {
    val M = DenseMatrix.zeros[Double](rows, cols)
    CsvReader().mapi (data, M) {
      (pair, line) => {
        val rowIdx:Int = pair._1
        rowIdx == 0 && skipHeader match {
          case true => pair._2
          case _ => {
            val mat:DenseMatrix[Double] = pair._2
            val temp:DenseMatrix[Double] = DenseMatrix.tabulate[Double](1,line.length) { case (i,j) => line(j).toDouble }
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

package au.id.cxd.math.data

import java.io.{File, FileInputStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

/**
  * A very simplistic csv reader which takes a file and a csv separator
  * Created by cd on 30/04/2016.
  */
class CsvReader(val separators: Array[Char] = Array[Char]('\t', ',')) extends TextReader {

  private def openFile(file: File) = {
    val inputStream = new FileInputStream(file)
    val buffer = new BufferedSource(inputStream)
    buffer
  }


  /**
    * read the file and pass each line into the supplied block
    *
    * @param file
    * @param blockFn
    * @tparam T
    * @return
    */
  protected def read[T](file: File, seed: T)(blockFn: (T, String) => T): T = {
    val buffer = openFile(file)
    val reader = buffer.bufferedReader()
    val result = buffer.getLines().foldLeft(seed)(blockFn)
    buffer.close
    result
  }

  /**
    * read data as CSV return a row based table with columns in each row.
    *
    * @param file
    * @return
    */
  def readCsv(file: File): ListBuffer[mutable.Buffer[String]] = {
    val result = ListBuffer[mutable.Buffer[String]]()
    read(file, result) {
      (accum, item) =>
        isComment(item.trim) match {
          case true => accum
          case _ => {
            val cols = item.trim.split(separators).toBuffer
            accum :+ cols
          }
        }
    }
  }


  /**
    * read data as CSV return a row based table with columns in each row.
    *
    * @param file
    * @param blockFn : f(T, mutable.Buffer[String]) => T
    *                the block function allows accumulation of each line in the Csv into the type T
    *                the block function recieves the T accumulator for each row in the CSV and a set of columns as the second parameter.
    *                It processes the columns and returns a second row.
    * @return
    */
  def readCsv[T](file: File, seed: T)(blockFn: (T, mutable.Buffer[String]) => T) = {
    read(file, seed) {
      (accum, item) =>
        isComment(item.trim) match {
          case true => accum
          case _ => {
            val cols = item.trim.split(separators).toBuffer
            blockFn(accum, cols)
          }
        }
    }
  }


  /**
    * map from the data set to the type T returned by the block function
    *
    * @param data
    * @param accum
    * @param blockFn
    * The blockFn takes the parameters
    * (Int,T), I2
    * The first paramter is a tuple of (Int, T)
    * which allows the row index to be sent as the first item
    * and the accumulator type of T to be supplied as the second item
    * and returns a type of T which is either the same instance of the accumulator or a new accumulator of same type.
    *
    * The block function allows the CsvReader to transform the data into the accumulator type T.
    * @tparam T
    * @return
    */
  def mapi[I1 <: Iterable[I2], I2 <: Iterable[S], S, T](data: I1, accum: T)(blockFn: ((Int, T), I2) => T) = {
    data.foldLeft((0, accum)) {
      (pair, row) =>
        (pair._1 + 1, blockFn(pair, row))
    }
  }

}

object CsvReader {
  def apply() = new CsvReader()
}

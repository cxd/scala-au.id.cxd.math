package au.id.cxd.math.data

import java.io.{File, FileInputStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

/**
  * A very simplistic csv reader which takes a file and a csv separator
  * Created by cd on 30/04/2016.
  */
class CsvReader(separators: Array[Char] = Array[Char]('\t', ',')) extends TextReader {

  private def openFile(file:File) = {
    val inputStream = new FileInputStream(file)
    val buffer = new BufferedSource(inputStream)
    buffer
  }


  /**
    * read the file and pass each line into the supplied block
    * @param file
    * @param blockFn
    * @tparam T
    * @return
    */
  private def read[T](file:File, seed:T)(blockFn: (T, String) => T):T = {
    val buffer = openFile(file)
    val reader = buffer.bufferedReader()
    val result = buffer.getLines().foldLeft(seed) (blockFn)
    buffer.close
    result
  }

  /**
    * read data as CSV return a row based table with columns in each row.
    * @param file
    * @return
    */
  def readCsv(file: File):ListBuffer[mutable.Buffer[String]] = {
    val result = ListBuffer[mutable.Buffer[String]]()
    read (file, result) {
      (accum, item) => isComment (item.trim) match {
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
    * @param file
    * @return
    */
  def readCsv[T](file: File, seed:T)(blockFn:(T,mutable.Buffer[String]) => T) = {
    read (file, seed) {
      (accum, item) => isComment (item.trim) match {
        case true => accum
        case _ => {
          val cols = item.trim.split(separators).toBuffer
          blockFn (accum, cols)
        }
      }
    }
  }


  /**
    * map from the data set to the type T returned by the block function
    * @param data
    * @param accum
    * @param blockFn
    * @tparam T
    * @return
    */
  def mapi[I1 <: Iterable[I2], I2 <: Iterable[S], S, T](data:I1, accum:T)(blockFn:((Int, T), I2) => T) = {
    data.foldLeft((0, accum)) {
      (pair, row) =>
        (pair._1 + 1, blockFn (pair, row) )
    }
  }

}

object CsvReader {
  def apply() = new CsvReader()
}

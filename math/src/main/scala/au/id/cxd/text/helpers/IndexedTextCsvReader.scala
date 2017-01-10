package au.id.cxd.text.helpers

import java.io.File

import au.id.cxd.math.data.CsvReader

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Read the CSV data and map identifier columns into a resulting map where
  * each row index of the Csv is returned for the corresponding set of identifier columns.
  * Note in the case where the idColumns is an empty sequence no row indexes will be returned.
  * Created by cd on 9/1/17.
  */
case class IndexedTextCsvReader(val skipHeader:Boolean = true, val idColumns: Seq[Int] = Seq[Int](), override val separators: Array[Char] = Array[Char]('\t', ','))
  extends CsvReader(separators) {



  /**
    * extract the set of ids from the row record.
    * @param data
    * @return
    */
  def extractIdAndData(data:mutable.Buffer[String]) = {
    data.zipWithIndex.foldLeft((Seq[String](), mutable.Buffer[String]())) {
      (accum, pair) => {
        val (indices, terms) = accum
        val (term, idx) = pair
        idColumns.contains(idx) match {
          case true => (indices :+ term, terms)
          case _ => (indices, terms :+ term)
        }
      }
    }
  }

  /**
    * read the CSV file, operate upon the headers if necessary and extract an index of
    * row identifiers. Remove dentifiers from the resulting CSV data.
    *
    * @param file
    * @return
    */
  def readAndIndexCsv(file: File): (mutable.Map[Int, Seq[String]], ListBuffer[mutable.Buffer[String]]) = {
    val result = ListBuffer[mutable.Buffer[String]]()
    val rowidx = skipHeader match {
      case true => -1
      case _ => 0
    }
    /**
      * a mutable map of indexed id columns
      */
    val idIndexMap: mutable.Map[Int, Seq[String]] = mutable.Map[Int, Seq[String]]()

    val (totalRows, idIndexMap1, result1) = read(file, (rowidx, idIndexMap, result)) {
      (accum, item) => {
        accum._1 < 0 match {
          case true => (0, accum._2, accum._3)
          case _ => {
            val cols = item.trim.split(separators).toBuffer[String].
              map { _.stripMargin('\'').stripSuffix("'") }.
              map { _.stripMargin('"').stripSuffix(""""""") }
            val (ids, data) = idColumns.size > 0 match {
              case true => extractIdAndData(cols)
              case _ => (Seq[String](), cols) // empty id columns
            }
            accum._2.put(accum._1, ids)
            (accum._1 + 1, accum._2, accum._3 :+ data)
          }
        }
      }
    }
    (idIndexMap1, result1)
  }

}

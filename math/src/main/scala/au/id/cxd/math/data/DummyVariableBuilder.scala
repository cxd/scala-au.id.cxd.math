package au.id.cxd.math.data

import breeze.linalg.DenseMatrix

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by cd on 3/05/2016.
  */
class DummyVariableBuilder(val columnName: String, val uniqueValues: Set[String], val columnValues: Seq[String]) {

  val uniqueList:List[String] = uniqueValues.toList

  /**
    * locate the index of the value from the set of unique values
    * @param value
    * @return
    */
  def indexOf(value:String) = {
    uniqueList.indexOf(value)
  }

  /**
    * given N unique values convert to dummy indicator variables
    * which for each $V_n \in N$ have 0 for values not equal to $V_n$
    * and 1 for values equal to $V_n$.
    * This results in $N$ columns.
    * This will result in a indicator matrix
    * where the ijth value represents the presence of column value i from the ordered set of unique values
    */
  def createDummyColumns():DenseMatrix[Double] = {
    // number of rows corresponds to the column values
    // number of columns corresponds to the number of indicator variables.
    DenseMatrix.tabulate[Double](columnValues.length, uniqueValues.toList.length) {
      case (i, j) => {
        val datum = columnValues(i).replaceAll("\"", "")
        indexOf(datum) match {
          case j => 1
          case _ => 0
        }
      }
    }

  }

}

object DummyVariableBuilder {

  /**
    * convert to dummy variables.
    *
    * @param columnName
    * @param uniqueValues
    * @param columnValues
    * @return
    */
  def apply(columnName: String, uniqueValues: Set[String], columnValues: Seq[String]) =
    new DummyVariableBuilder(columnName.replaceAll("\"",""), uniqueValues, columnValues)

  /**
    * for each column in the file create a
    * dummy variable builder that has the column name
    * and the set of corresponding unique values for each column
    */
  def extractColumns(headers: mutable.Buffer[String], rows: Seq[mutable.Buffer[String]]): List[DummyVariableBuilder] = {
    // TODO: change this approach to use iteration instead.
    // this is a bottleneck when loading large volumes of data.
    val results = rows.par.foldLeft (TreeMap[String,ListBuffer[String]]()) {
      (accum, row) => {
        val accum1 = headers.foldLeft((0, accum)) {
          (pair, col) => {
            val idx = pair._1
            val accum = pair._2
            accum.contains (col) match {
              case true => {
                val items = accum.get(col).get :+ row(idx)
                val update = accum - col
                (idx + 1, update + (col -> items))
              }
              case _ => (idx + 1, accum + (col -> ListBuffer[String](row(idx))))
            }
          }
        }
        accum1._2
      }
    }
    /*
    val results = headers.foldLeft((0, Map[String, ListBuffer[String]]())) {
      (pair, header) => {
        val idx = pair._1
        val accum = pair._2
        // the set operation will extract the unique values from each of the columns.
        // duplicates are automatically discarded.
        val columns = rows.foldLeft(ListBuffer[String]()) {
          (cols, row) => {
            cols :+ row(idx)
          }
        }
        (idx + 1, accum + (header -> columns))
      }
    }
    */

    // potentially use iteration instead of fold



    val vars = results.keys map {
      key => {
        val values = results.getOrElse(key, { List[String]() })
        // the toSet assigns the unique values to the variable builder.
        // potentially a more optimised method of finding unique values could be used.
        // such as a trie.
        apply(key, values.toSet, values)
      }
    }
    vars.toList
  }


  /**
    * given the set of input data concatenate the matrices togethor.
    * @param headers
    * @param rows
    * @return
    */
  def buildIndicatorMatrix(headers:mutable.Buffer[String], rows:Seq[mutable.Buffer[String]]) = {
    val builders = extractColumns(headers, rows)
    val matrices = builders map { builder => builder.createDummyColumns() }
    val result = matrices.reduce {
      (a, b) => DenseMatrix.horzcat(a, b)
    }
    val keyedMappings = builders.foldLeft(Map[String,Set[String]]()) {
      (accum, builder) => accum + (builder.columnName -> builder.uniqueValues)
    }
    (result, keyedMappings)
  }


}

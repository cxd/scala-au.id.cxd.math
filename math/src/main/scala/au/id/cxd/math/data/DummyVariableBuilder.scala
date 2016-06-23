package au.id.cxd.math.data

import breeze.linalg.DenseMatrix

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by cd on 3/05/2016.
  */
class DummyVariableBuilder(val columnName: String, val uniqueValues: Set[String], val columnValues: Seq[String]) {

  import au.id.cxd.math.data.DummyVariableBuilder._

  val uniqueList: List[String] = uniqueValues.toList

  /**
    * locate the index of the value from the set of unique values
    *
    * @param value
    * @return
    */
  def indexOf(value: String) = {
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
  def createDummyColumns(): DenseMatrix[Double] = {
    // number of rows corresponds to the column values
    // number of columns corresponds to the number of indicator variables.
    DenseMatrix.tabulate[Double](columnValues.length, uniqueValues.toList.length) {
      case (i, j) => {
        val datum = dataFor(columnValues, i)
        indexOf(datum) match {
          case j => 1
          case _ => 0
        }
      }
    }

  }


  /**
    * create a single row where the matching value of that row
    * contains 1 and the other columns 0.
    *
    * @param row
    * @return
    */
  def createDummyRow(row: Int): DenseMatrix[Double] = {
    val datum = dataFor(columnValues, row)
    val idx = indexOf(datum)
    DenseMatrix.tabulate[Double](1, uniqueValues.toList.length) {
      case (i, j) => {
        idx == j match {
          case true => 1
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
    new DummyVariableBuilder(columnName.replaceAll("\"", ""), uniqueValues, columnValues)


  def dataFor(row: Seq[String], idx: Int) = row(idx).trim.replaceAll("\"", "").replaceAll(" ", "") match {
    case "" => "NONE"
    case other => other
  }

  /**
    * for each column in the file create a
    * dummy variable builder that has the column name
    * and the set of corresponding unique values for each column
    */
  def extractColumns(headers: mutable.Buffer[String], rows: Seq[mutable.Buffer[String]]): List[DummyVariableBuilder] = {

    val results = rows.par.foldLeft(TreeMap[String, ListBuffer[String]]()) {
      (accum, row) => {
        val accum1 = headers.foldLeft((0, accum)) {
          (pair, col) => {
            val idx = pair._1
            val accum = pair._2
            accum.contains(col) match {
              case true => {
                // mutable update
                val datum = dataFor(row, idx)
                accum.get(col).get.append(datum)
                (idx + 1, accum)
              }
              case _ => (idx + 1, accum + (col -> ListBuffer[String](dataFor(row, idx))))
            }
          }
        }
        accum1._2
      }
    }

    val vars = results.keys map {
      key => {
        val values = results.getOrElse(key, {
          List[String]()
        })
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
    *
    * @param headers
    * @param rows
    * @return
    */
  def buildIndicatorMatrix(headers: mutable.Buffer[String], rows: Seq[mutable.Buffer[String]]) = {
    val builders = extractColumns(headers, rows)
    // TODO: the order needs to be swapped
    // each builder represents an set of dummy columns k
    // within the builder it iterates rows,
    // instead we need to iterate rows first, and then iterate the builders
    // then construct a matrix out of the each row provided by the builders

    val totalColumns = builders.foldLeft(0) { (n, builder) => n + builder.uniqueValues.size }
    val totalRows = rows.size

    val matrices = (for (i <- 0 to totalRows) yield i).map {
      n =>
        val cols = builders map { builder => builder.createDummyRow(n) }
        cols.reduce { (a, b) => DenseMatrix.horzcat(a, b) }
    }
    // convert the rows into a single matrix
    // the order of traversal is reversed
    val result = matrices.reduce { (a, b) => DenseMatrix.vertcat(a, b) }

    /*
    val matrices = builders map { builder => builder.createDummyColumns() }
    val result = matrices.reduce {
      (a, b) => DenseMatrix.horzcat(a, b)
    }
    */
    val keyedMappings = builders.foldLeft(Map[String, Set[String]]()) {
      (accum, builder) => accum + (builder.columnName -> builder.uniqueValues)
    }
    (result, keyedMappings)
  }


}

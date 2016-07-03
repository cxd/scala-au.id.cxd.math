package au.id.cxd.math.data

import java.util.concurrent.Executors

import au.id.cxd.math.parallel.LinearScaleExecutionContext
import breeze.linalg.DenseMatrix

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

/**
  * The dummy variable builder class is used to extract the create the columns of
  * 0 or 1 corresponding to the row associated with the value
  * the order of unique values determine the sequence of 0 or 1 indicators.
  *
  * The column values are converted into vectors before being used to lookup the data.
  * The vector has an almost constant apply() method as opposed to other sequences which are Linear.
  * http://docs.scala-lang.org/overviews/collections/performance-characteristics
  *
  *
  * Created by cd on 3/05/2016.
  */
class DummyVariableBuilder(val columnName: String, val uniqueValues: Set[String], val columnValues: Vector[String]) {

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
        indicator(j, datum)
      }
    }

  }


  /**
    * create a single row where the matching value of that row
    * contains 1 and the other columns 0.
    *
    * the value of the row will determine which of the unique values
    * arranged in column order will be 1 or 0
    *
    * @param row
    * @return
    */
  def createDummyRow(row: Int): DenseMatrix[Double] = {
    val datum = dataFor(columnValues, row)
    val idx = indexOf(datum)
    DenseMatrix.tabulate[Double](1, uniqueValues.toList.length) {
      case (i, j) => indicator(j, datum)
    }
  }

  /**
    * the indicator value for the datum defined at the position
    * for the column
    * if the datum corresponds to the unique value at the index defined by the column
    * return 1
    * otherwise return 0
    *
    * @param column
    * @param datum
    * @return
    */
  def indicator(column: Int, datum: String): Double = {
    val idx = indexOf(datum)
    idx == column match {
      case true => 1
      case _ => 0
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
    new DummyVariableBuilder(columnName.replaceAll("\"", ""), uniqueValues, columnValues.toVector)


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
  def buildIndicatorMatrix(headers: mutable.Buffer[String], rows: Seq[mutable.Buffer[String]]):Future[Option[(DenseMatrix[Double], Map[String, Set[String]])]] = {
    val builders = extractColumns(headers, rows)
    // each builder represents an set of dummy columns k
    // within the builder it iterates rows,
    // instead we need to iterate rows first, and then iterate the builders
    // then construct a matrix out of the each row provided by the builders

    val totalColumns = builders.foldLeft(0) { (n, builder) => n + builder.uniqueValues.size }
    val totalRows = rows.size
    // we need to create a sequence of column indices which are mapped to each builder for that column
    // when combined with the other builders
    val (totalCols, mappings) = builders.foldLeft((0, Seq[(Int, DummyVariableBuilder)]())) {
      (accum, builder) => {
        val n = accum._1
        val pairs = accum._2
        val subset = (for (i <- 0 until builder.uniqueValues.size) yield (n + i, builder))
        (n + builder.uniqueValues.size, pairs ++ subset)
      }
    }

    val cache = mutable.HashMap[Int, DummyVariableBuilder]()
    def findMapping(column: Int, mappings: Seq[(Int, DummyVariableBuilder)]) = {
      cache.contains(column) match {
        case true => cache.get(column).get
        case _ => {
          val builder = mappings.filter { pair => pair._1 == column }.head._2
          cache.put(column, builder)
          builder
        }
      }
    }

    /***
      * use the vector contained within the builder to retrieve the value at the supplied index
      * @param i
      * @param j
      * @return
      */
    def valueFor(i: Int, j: Int): Double = {
      val builder = findMapping(j, mappings)
      val datum = dataFor(builder.columnValues, i)
      builder.indicator(j, datum)
    }

    // the process each row we will do this in parallel
    // however we want the maximum number of threads to be bounded
    //
    implicit val ec = LinearScaleExecutionContext(4)


    // cannot obviously execute all futures in parallel
    // but can we build the sequence in parallel and wait for the result.
    // maybe it would be better to do this for each row
    // then combine the results into a single sequence?
    // unfortunately at some point it needs to be converted into a matrix
    val mat = Future.sequence {
      for (i <- 0 until totalRows) yield Future {
          for (j <- 0 until totalCols) yield valueFor(i, j)
        }
    }.map {
      subsequences => subsequences.flatten
    }.map {
      values => new DenseMatrix(totalRows, totalCols, values.toArray)
    }.map {
      result => {
        val keyedMappings = builders.foldLeft(Map[String, Set[String]]()) {
          (accum, builder) => accum + (builder.columnName -> builder.uniqueValues)
        }
        Some(result, keyedMappings)
      }
    }
    mat
  }


}

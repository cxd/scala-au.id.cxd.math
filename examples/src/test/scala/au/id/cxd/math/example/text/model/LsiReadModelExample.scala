package au.id.cxd.math.example.text.model

import java.io.File

import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.example.text.model.LsiModelExample.getClass
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.model.LatentSemanticIndex.LsiSearchSpace

import scala.util.{Failure, Success}

/**
  * This example demonstrates how to read the LSI model from file
  * and use it to perform a search.
  *
  *
  * The process to build the SVD may take up to over 3gb of RAM once completed it will then free memory back around 1gb.
  * This is due to the moderate data size. Obviously depending on the volume of data, RAM and CPU will affect performance.
  * The algorithm is not a distributed algorithm and computes in process.
  * For this example make sure to add java arguments for -Xmx4g -Xms1g
  *
  * Note typically, if storing an LSI model the server would load it at startup
  * and build the search space, this would then be retained in memory for the lifetime of the server.
  *
  * However this example demonstrates simply how to read from an archive that has been stored using the builtin
  * approach, and then provides an example of how to use it for a search query.
  *
  * The default storage mechanism of CSV is a bottleneck and data does take sometime to read into memory.
  * More efficient methods such as binary serialization could be used rather than the default methods for storage.
  * The CSV format is used for the sake of being able to examine the data with external tools such as R.
  *
  * Note the SVD can be used to explore components and to graph scatter plots of projections into those components.
  * This will be treated in a separate example.
  *
  * Created by cd on 13/1/17.
  */
object LsiReadModelExample {

  val targetZip = "lsiexample.zip"

  val textData = "example_text_corpus_data.csv"

  val defaultQuery = Array("http", "redirect", "error")


  def loadDocuments() = {
    val url = getClass.getClassLoader().getResource(textData)
    val inputCsv = url.getFile
    val data = new CsvReader().readCsv(new File(inputCsv))
    data
  }

  def readModel() = {
    val readResult = LatentSemanticIndex.readZip(targetZip)
    readResult match {
      case Success(lsi2) => {
        println(s"Dimensions: U (${lsi2.svD.U.rows} x ${lsi2.svD.U.cols}) S: ${lsi2.svD.S.length} Vt: (${lsi2.svD.Vt.rows} x ${lsi2.svD.Vt.cols})")
        Some(lsi2)
      }
      case Failure(error) => {
        println(s"Failed to read $targetZip due to ${error}")
        None
      }
    }
  }

  /**
    * load the search space from the lsi
    * note this search space is generally going to be loaded
    * at startup and kept in memory
    * @param lsi
    * @return
    */
  def loadSearchSpace(lsi:LatentSemanticIndex) = {
    val (ssU, ssS, ssVt) = LatentSemanticIndex.makeSearchSpace(lsi)

    println(s"Search Space U dimension: ${ssU.rows} x ${ssU.cols}")
    println(s"Search Space V dimension: ${ssVt.rows} x ${ssVt.cols}")

    Some((ssU, ssS, ssVt), lsi)
  }


  def performSearch(query: Array[String])(search:(LsiSearchSpace, LatentSemanticIndex)) = {
    val loader = EmbeddedStopwordsLoader()
    val stopwords = loader.load()

    val (ssU, ssS, ssVt) = search._1
    val lsi = search._2

    // Note in a production system
    val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopwords, lsi)

    // Note:
    val data = loadDocuments()
    // find the data that matches the search results.
    val results = searchResults.take(10).foldLeft(Seq[(String, String)]()) {
      (accum, result) => {
        val ids = result._3
        val id = ids(1)
        val matchRecord = data.find { row => row(1).equalsIgnoreCase(id) }
        matchRecord match {
          case Some(row) => accum :+ (row(1), row(2))
          case _ => accum
        }
      }
    }
    println(s"First 10 Search Results")
    println(results)
  }



  def main(args: Array[String]) = {

    val query = args.length > 0 match {
      case true => args(0).split(" ")
      case _ => defaultQuery
    }

    readModel().flatMap (loadSearchSpace).foreach {
      search => performSearch(query)(search)
    }

  }
}

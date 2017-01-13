package text.count

import java.io.File

import au.id.cxd.math.data.CsvReader
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.{LatentSemanticIndex, LsiDocumentCluster}
import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.util.Success

/**
  * Created by cd on 12/1/17.
  */
class TestLsi extends FlatSpec with ShouldMatchers {

  "LSI" should "build model" in {
    val input = "subset_text_input.csv"
    val url = getClass.getClassLoader().getResource(input)
    val inputCsv = url.getFile
    val idCols = Seq(0, 1)
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols)
    entropy should not be (0.0)
    println(s"Entropy: $entropy")
    contributions.length should not be (0)
    println(s"Contributions: $contributions")

    println(s"Dimensions: U (${lsi.svD.U.rows} x ${lsi.svD.U.cols}) S: ${lsi.svD.S.length} Vt: (${lsi.svD.Vt.rows} x ${lsi.svD.Vt.cols})")

    // Note: prior to performing the search we can make use of the entropy and contributions to select k dimensions from the search space.
    // at this stage we have not done that.

    val (ssU, ssS, ssVt) = LatentSemanticIndex.makeSearchSpace(lsi)

    println(s"Search Space U dimension: ${ssU.rows} x ${ssU.cols}")
    println(s"Search Space V dimension: ${ssVt.rows} x ${ssVt.cols}")

    val loader = EmbeddedStopwordsLoader()
    val stopwords = loader.load()
    val query = Array("http", "redirect", "error")


    // TODO: debug the search process and the search projection
    val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopwords, lsi)

    searchResults.length should not be (0)
    println(s"First 10 Search Results")
    println(searchResults.take(10))

    val data = new CsvReader().readCsv(new File(inputCsv))
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

    // TODO: write tests for saving and loading the LSI.
  }

  /**
    * test writing and then reading the model.
    */
  "LSI" should "write and read model" in {
    val input = "subset_text_input.csv"
    val url = getClass.getClassLoader().getResource(input)
    val inputCsv = url.getFile
    val idCols = Seq(0, 1)
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols)
    entropy should not be (0.0)
    println(s"Entropy: $entropy")
    contributions.length should not be (0)
    println(s"Contributions: $contributions")

    println(s"Dimensions: U (${lsi.svD.U.rows} x ${lsi.svD.U.cols}) S: ${lsi.svD.S.length} Vt: (${lsi.svD.Vt.rows} x ${lsi.svD.Vt.cols})")

    // now test writing the model.
    val writeResult = LatentSemanticIndex.writeTemp(lsi)("testlsi.zip")

    writeResult match {
      case Success(flag) => flag should be(true)
      case _ => fail("Failed to write testlsi.zip")
    }

    val readResult = LatentSemanticIndex.readZip("testlsi.zip")
    readResult match {
      case Success(lsi2) => {
        lsi2.docIdMap.size should equal(lsi.docIdMap.size)
        lsi2.colTermMap.size should equal(lsi.colTermMap.size)


        println(s"Dimensions: U (${lsi2.svD.U.rows} x ${lsi2.svD.U.cols}) S: ${lsi2.svD.S.length} Vt: (${lsi2.svD.Vt.rows} x ${lsi2.svD.Vt.cols})")

        // test searching against the loaded model

        val (ssU, ssS, ssVt) = LatentSemanticIndex.makeSearchSpace(lsi2)

        println(s"Search Space U dimension: ${ssU.rows} x ${ssU.cols}")
        println(s"Search Space V dimension: ${ssVt.rows} x ${ssVt.cols}")

        val loader = EmbeddedStopwordsLoader()
        val stopwords = loader.load()
        val query = Array("http", "redirect", "error")


        // TODO: debug the search process and the search projection
        val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopwords, lsi2)

        searchResults.length should not be (0)
        println(s"First 10 Search Results")
        println(searchResults.take(10))


      }
    }

  }


  /**
    * a test for basic clustering.
    */
  "LSI" should "allow clustering" in {
    val input = "subset_text_input.csv"
    val url = getClass.getClassLoader().getResource(input)
    val inputCsv = url.getFile
    val idCols = Seq(0, 1)
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols)

    // lets use maximum of 50 clusters for this example
    val k = 20
    val clusterLsi = new LsiDocumentCluster {}

    val docClust = clusterLsi.clusterDocuments(lsi, k)

    val data = new CsvReader().readCsv(new File(inputCsv))

    println("Document Clusters")

    docClust.foreach {
      docPair => {
        val cluster = docPair._1
        val cnt = docPair._2.length
        // sort the examples from maximum component value to least
        val examples = docPair._2.sortBy(-_._4).map {
          doc => {
            // index x docIds x Component x ComponentWeight
            // (Int , Seq[String], Int, Double )
            val id = doc._2(1)
            val matchRecord = data.find { row => row(1).equalsIgnoreCase(id) }
            matchRecord match {
              case Some(row) => row(2)
              case _ => ""
            }
          }
        }
        val debug = examples.length > 10 match {
          case true => examples.take(10)
          case _ => examples
        }
        println("----------------------------------")
        println(s"Cluster: $cluster Size: $cnt")
        debug.foreach(item => println(item))
        println("----------------------------------")
      }
    }

    println("Attribute Clusters")
    val termClust = clusterLsi.clusterAttributes(lsi, k)

    // for the terms lets print out the first 10 terms of highest magniture in each cluster.
    termClust.foreach {
      termPair => {
        val cnt = termPair._2.length
        val terms = termPair._2.sortBy(-_._4)
        val debug = terms.length > 10 match {
          case true => terms.take(10)
          case _ => terms
        }

        println("----------------------------------")
        println(s"Cluster: ${termPair._1} Size: $cnt")
        debug.foreach(item => println(item._2._1))
        println("----------------------------------")
      }
    }

  }

}

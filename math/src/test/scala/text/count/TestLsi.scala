package text.count

import java.io.File

import au.id.cxd.math.data.CsvReader
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.LatentSemanticIndex
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by cd on 12/1/17.
  */
class TestLsi extends FlatSpec with ShouldMatchers {

  "LSI" should "build model" in {
    //val input = "example_input_data.csv"
    val input = "subset_text_input.csv"
    val url = getClass.getClassLoader().getResource(input)
    val inputCsv = url.getFile
    val idCols = Seq(0,1)
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols)
    entropy should not be(0.0)
    println(s"Entropy: $entropy")
    contributions.length should not be (0)
    println(s"Contributions: $contributions")

    println(s"Dimensions: U (${lsi.svD.U.rows} x ${lsi.svD.U.cols}) S: ${lsi.svD.S.length} Vt: (${lsi.svD.Vt.rows} x ${lsi.svD.Vt.cols})")

    val (ssU, ssS, ssVt) = LatentSemanticIndex.makeSearchSpace(lsi)

    println(s"Search Space U dimension: ${ssU.rows} x ${ssU.cols}")
    println(s"Search Space V dimension: ${ssVt.rows} x ${ssVt.cols}")

    val loader = EmbeddedStopwordsLoader()
    val stopwords = loader.load()
    val query = Array("http", "redirect", "error")

    // TODO: debug the search process and the search projection
    val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopwords, lsi)

    searchResults.length should not be(0)
    println(s"First 10 Search Results")
    println(searchResults.take(10))

    val data = new CsvReader().readCsv(new File(inputCsv))
    // find the data that matches the search results.
    val results = searchResults.take(10).foldLeft(Seq[(String,String)]()) {
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

}

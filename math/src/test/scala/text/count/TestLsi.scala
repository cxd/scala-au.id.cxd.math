package text.count

import java.io.File

import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.LatentSemanticIndex
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by cd on 12/1/17.
  */
class TestLsi extends FlatSpec with ShouldMatchers {

  "LSI" should "build model" in {
    val url = getClass.getClassLoader().getResource("subset_text_input.csv")
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
    val query = Array("http", "error", "code")

    // TODO: debug the search process and the search projection
    val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopwords, lsi)

    searchResults.length should not be(0)
    println(s"First 10 Search Results")
    println(searchResults.take(10))

    // TODO: write tests for saving and loading the LSI.
  }

}

package text.count

import java.io.File

import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.{EmbeddedStopwordsLoader, IndexedTextCsvReader}
import au.id.cxd.text.preprocess.StemmingPatternFilter
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by cd on 8/1/17.
  */
class TestTfIdfCount extends FlatSpec with Matchers {

  /**
    * test reading some of the apache defect data
    * it has the columns
    * project,key,text
    * we skip the header
    * and extract the indexes for project,key
    * and return the set of lines in text
    * @return
    */
  def readData() = {
    val url = getClass.getClassLoader().getResource("subset_text_input.csv")
    val file = new File(url.getFile)
    IndexedTextCsvReader(skipHeader=true, idColumns=Seq(0,1)).readAndIndexCsv(file)
  }

  "TFIDF" should "produce term map and tfidf matrix" in {
    val (idMap, lines) = readData()
    idMap.size should not be(0)

    lines.length should equal(idMap.size)
    println(s"Total lines: ${lines.length}")

    val loader = EmbeddedStopwordsLoader()
    val stopwords = loader.load()

    stopwords.length should not be(0)
    println(s"Loaded ${stopwords.length} stop words")

    val stemmer = new StemmingPatternFilter(stopwords)
    val stemmedTerms = stemmer.filter(lines.map {_.head })

    val tfIdfCounter = TfIdfCount()

    // the columnMap has (columnIdx, (Term, HashCode))
    // the TfIdf is the matrix containing calculated weights for each document.
    val (columnMap, tfIdf) = tfIdfCounter.count(stemmedTerms)

    println(s"Extracted ${columnMap.size} unique terms")
    println(s"TF-IDF matrix dimension (${tfIdf.rows}, ${tfIdf.cols})")

    columnMap.size should not be(0)
    tfIdf.rows should not be(0)
    tfIdf.cols should not be(0)
  }

}

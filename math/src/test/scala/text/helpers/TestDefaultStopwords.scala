package text.helpers

import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by cd on 7/1/17.
  */
class TestDefaultStopwords extends FlatSpec with ShouldMatchers {

   "Default stop words" should "load" in {
     val loader = EmbeddedStopwordsLoader()
     val stopwords = loader.load()
     stopwords.length should not be(0)
     println(s"Loaded ${stopwords.length} stop words")
   }

}

package au.id.cxd.text.helpers

import scala.io.Source
import scala.util.Try

/**
  * Created by cd on 7/1/17.
  */
case class EmbeddedStopwordsLoader() extends StopwordsLoader {

  private val stopwords = "stop-word-list.txt"

  private def loadStopwords () = Try {
    val url = getClass.getClassLoader.getResource(stopwords)
    val source = Source.fromFile(url.getFile)
    source.getLines().toSeq
  } toOption

  /**
    * load the set of stopwords.
    *
    * @return
    */
  def load(): Seq[String] = loadStopwords() match {
    case Some(data) => data
    case _ => Seq()
  }
}

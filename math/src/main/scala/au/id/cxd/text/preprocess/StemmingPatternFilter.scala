package au.id.cxd.text.preprocess

/**
  * Filter each line via a pattern and then tokenise by stemming each token
  * Created by cd on 6/1/17.
  */
class StemmingPatternFilter(override val stopWords: Seq[String],
                            override val pattern: String = """[,\.\!\?\s]""") extends StopwordPatternFilter(stopWords, pattern) {
  val regex = """\d""".r

  override def tokenise(line: String): Array[String] =
    super.tokenise(line).map {
      p: String => regex.replaceAllIn(p, "")
    }
      .filter(!_.isEmpty)
      .filter(!_.contains("_"))
      .map {
        p => PorterStemmer(p)
      }

}

package au.id.cxd.text.preprocess

/**
  * Created by cd on 7/1/17.
  */
class StopwordPatternFilter ( val stopWords:Seq[String],
                              override val pattern:String = """[,\.\!\?\s]""") extends LinePatternFilter (pattern) {

  /**
    * tokenise and filter the stop words
    * @param line
    * @return
    */
  override def tokenise(line: String): Array[String] =
    super.tokenise(line).filter {
      item => stopWords.filter(other => other.equalsIgnoreCase(item)).isEmpty
    }

}

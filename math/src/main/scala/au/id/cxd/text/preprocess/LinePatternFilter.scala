package au.id.cxd.text.preprocess


/**
  * Created by cd on 6/1/17.
  */
class LinePatternFilter(val pattern: String = """[,\.\!\?\s]""") extends StringSeqFilter {


  /**
    * tokenise the line
    *
    * @param line
    * @return
    */
  def tokenise(line: String) = line.split(pattern).map {
    item => item.replaceAll("""\W""", "").toLowerCase
  }.filter(!_.isEmpty)
   .filter(!_.contains("_"))

  /**
    * tokenise a single query instance
    *
    * Note this does not change
    *
    * @param query
    * @return
    */
  def tokeniseQuery(query: Array[String]): Array[String] = query.map {
    item => item.replaceAll("""\W""", "").toLowerCase
  }.filter(!_.isEmpty)
    .filter(!_.contains("_"))
}

object LinePatternFilter {
  def apply(pattern: String = """[,\.\!\?\s]""") = new LinePatternFilter(pattern)
}
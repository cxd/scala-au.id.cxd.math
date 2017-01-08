package au.id.cxd.text.preprocess


/**
  * Created by cd on 6/1/17.
  */
class LinePatternFilter(val pattern:String = """[,\.\!\?\s]""") extends StringSeqFilter {


  /**
    * tokenise the line
    * @param line
    * @return
    */
  def tokenise(line:String) = line.split(pattern).map {
    item => item.replaceAll("""\W""", "").toLowerCase
  }

}

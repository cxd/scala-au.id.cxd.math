package au.id.cxd.text.preprocess

/**
  * Created by cd on 7/1/17.
  */
trait StringSeqFilter {


  /**
    * the main tokenisation block
    * @param line
    * @return
    */
  def tokenise(line:String):Array[String]


  /**
    * filter each line in the string sequence and produce
    * the output sequence
    * @param lines
    * @return
    */
  def filter (lines:Seq[String]):Seq[Array[String]] = lines.map { tokenise(_) }

}

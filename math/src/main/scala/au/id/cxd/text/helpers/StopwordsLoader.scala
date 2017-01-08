package au.id.cxd.text.helpers

/**
  * Created by cd on 7/1/17.
  */
trait StopwordsLoader {

  /**
    * load the set of stopwords.
    * @return
    */
  def load():Seq[String]
}

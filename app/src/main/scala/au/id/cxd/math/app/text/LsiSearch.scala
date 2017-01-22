package au.id.cxd.math.app.text

import au.id.cxd.math.app.workflow.{ParameterisedWorkflowStep, ProjectWorkflowStep, Workflow}
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.model.LatentSemanticIndex.LsiSearchSpace
import au.id.cxd.text.preprocess.LinePatternFilter

import scala.collection.mutable

/**
  * Created by cd on 22/1/17.
  */
class LsiSearch(val lsiP:LsiProject)
  extends ParameterisedWorkflowStep[String, (mutable.Buffer[(Int, Double, Seq[String])], LsiProject)] {

  lazy val searchSpace:LsiSearchSpace = LsiSearch.readSearchSpace (lsiP)

  lazy val stopWords:Seq[String] = {
    val loader = EmbeddedStopwordsLoader()
    val words = loader.load()
    words
  }

  def makeQuery(text:String) = {
    val filter = LinePatternFilter()
    filter.tokenise(text)
  }


  def performSearch(query: Array[String]):mutable.Buffer[(Int, Double, Seq[String])] = {

    val (ssU, ssS, ssVt) = searchSpace
    val lsi = lsiP.workingModel

    val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopWords, lsi)
    searchResults
  }

  override def runWorkflow(searchText:String): Workflow[(mutable.Buffer[(Int, Double, Seq[String])], LsiProject)] = {
    val query = makeQuery(searchText)
    val results = performSearch (query)
    Workflow((results, lsiP))
  }
}

object LsiSearch {
  /**
    * cached search space.
    */
  def readSearchSpace(lsiP:LsiProject):LsiSearchSpace = LatentSemanticIndex.makeSearchSpace(lsiP.workingModel)

  def apply(lsiP:LsiProject) = new LsiSearch(lsiP)
}


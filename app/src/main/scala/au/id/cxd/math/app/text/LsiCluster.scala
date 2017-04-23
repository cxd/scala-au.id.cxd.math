package au.id.cxd.math.app.text

import au.id.cxd.math.app.workflow.{ProjectWorkflowStep, Workflow}
import au.id.cxd.text.model.LsiComponentCluster.{LsiAttributeCluster, LsiDocumentCluster}
import au.id.cxd.text.model.{LatentSemanticIndex, LsiComponentCluster}

/**
  * Created by cd on 23/1/17.
  */
class LsiCluster extends ProjectWorkflowStep[LsiProject] {


  /**
    * create document clusters
    *
    * @param lsi
    */
  def makeDocumentClusters(lsi: LatentSemanticIndex, cluster: LsiComponentCluster, k: Int) = {
    val clusters = cluster.clusterDocuments(lsi, k)
    clusters
  }

  /**
    * create the term clusters
    *
    * @param lsi
    * @param cluster
    * @param k
    * @return
    */
  def makeTermClusters(lsi: LatentSemanticIndex, cluster: LsiComponentCluster, k: Int) = {
    val clusters = cluster.clusterAttributes(lsi, k)
    clusters
  }

  /**
    * build the clusters.
    * @param model
    * @param kComponents
    * @return
    */
  def buildClusters (model:LatentSemanticIndex, kComponents:Int):(LsiAttributeCluster, LsiDocumentCluster) = {
    val cluster = new LsiComponentCluster {}
    val docClusters = makeDocumentClusters(model, cluster, kComponents)
    val termClusters = makeTermClusters(model, cluster, kComponents)
    (termClusters, docClusters)
  }

  /**
    * take the clustered terms and sort them by weighting for the cluster component.
    * Return a list of terms for the cluster.
    * @param lsi
    * @param clusters
    * @return
    */
  def mapTermsToCluster(lsi: LatentSemanticIndex, clusters: LsiAttributeCluster): Map[Int, Array[(Int, String, Double)]] =
    clusters.map {
      termPair => {
        val cluster = termPair._1
        val cnt = termPair._2.length
        val terms = termPair._2.sortBy(-_._4)
        //(Cluster x TermColumnIndex x Term x Weight)
        // map it to
        // (ColumnIndex, TermString, ComponentWeight)
        val mappedTerms = terms.map { term =>
          (term._2, term._3._1, term._4)
        }
        (cluster, mappedTerms)
      }
    }

  override def runWorkflow(): Workflow[LsiProject] = ???
}

package au.id.cxd.text.model

import breeze.linalg.DenseMatrix

/**
  * The LsiDocumentCluster approach uses the selected set of k components
  * in order to generate k clusters.
  *
  * Documents are associated with clusters by the use of the U matrix.
  *
  * Terms are associated with clusters by the use of the Vt matrix
  *
  * If there are k components, the U matrix has dimension (m x k)
  *
  * where m is the number of documents.
  *
  * And Vt matrix has the dimension (k x n) where n is the number of attributes.
  *
  * Additionally the correlation matrices for attributes can be used to select
  * highly correlated attributes once they have been clusered.
  *
  * This produces a cluster labelling for documents and terms based on the components defined in S.
  *
  * Created by cd on 13/1/17.
  */
trait LsiComponentCluster {

  /**
    * subtract the minimum value of the matrix
    *
    * @param M
    * @return
    */
  def subtractMinimum(M: DenseMatrix[Double]) = {
    val minVal = M.data.min
    M - minVal
  }

  /**
    * cluster the documents by determining the maximum U values in the U matrix.
    * The U matrix is first processed by subtracting the most negative value so that all values are positive.
    *
    * @param lsi
    * @return
    * This will return a set of documents each grouped by the cluster to which they correspond.
    * (ClusterId, Seq[(Int, Seq[String], Int)])
    */
  def clusterDocuments(lsi: LatentSemanticIndex, kClusters: Int):Map[Int, Array[(Int, Seq[String], Int, Double)]] = {
    // we select the first kClusters from U
    val U = lsi.svD.U(::, 0 until kClusters).toDenseMatrix
    // subtract the minimum value
    val U2 = subtractMinimum(U)
    // for each row in the documents we map this to the column which has the maximum value.
    lsi.docIdMap.map {
      docPair => {
        val idx = docPair._1
        val row = U2(idx, ::).inner
        // which column in the row has the maximum value
        val maxIdx = row.toArray.foldLeft((-1.0, 0, 0)) {
          (pair, item) => {
            item > pair._1 match {
              case true => (item, pair._3, pair._3 + 1)
              case _ => (pair._1, pair._2, pair._3 + 1)
            }
          }
        }
        // if the value in the original matrix is negative then we mark the component as negative
        // otherwise we mark it as positive.
        val clustId = maxIdx._2
        (docPair._1, docPair._2, clustId, maxIdx._1)
      }
    }.toArray
      .sortBy(_._1)
      .groupBy(_._3)
  }

  /**
    * cluster similar attributes
    * The value of the V matrix for each cluster is retained so that
    * it is possible to sort groupings of terms in order of the value.
    * @param lsi
    * @param kClusters
    * @return
    *
    * The output of the map has the key for the corresponding cluster.
    *
    * Whereas each entry in the group for the cluster is a tuple consisting of
    * (Cluster x TermColumnIndex x Term x Weight)
    */
  def clusterAttributes(lsi: LatentSemanticIndex, kClusters: Int):Map[Int, Array[(Int, Int,(String, Int, Int), Double)]] = {
    // we select the first kClusters from Vt and transpose it
    // so that the dimensions are (n, k) - n attributes x k columns
    val V = lsi.svD.Vt(0 until kClusters, ::).toDenseMatrix.t
    // subtract the minimum value
    val V2 = subtractMinimum(V)
    // now for each term index we map it into a cluster
    lsi.colTermMap.map {
      termPair => {
        val idx = termPair._1
        val row = V2(idx, ::).inner
        // which column in the row has the maximum value
        val maxIdx = row.toArray.foldLeft((-1.0, 0, 0)) {
          (pair, item) => {
            item > pair._1 match {
              case true => (item, pair._3, pair._3 + 1)
              case _ => (pair._1, pair._2, pair._3 + 1)
            }
          }
        }
        // if the value in the original matrix is negative then we mark the component as negative
        // otherwise we mark it as positive.
        val clustId = maxIdx._2
        // (Cluster x TermColumnIndex x Term x Weight)
        (clustId, termPair._1, termPair._2, maxIdx._1)
      }
    }.toArray
      .sortBy(_._2)
      .groupBy(_._1)
  }

}

object LsiComponentCluster extends LsiComponentCluster {

  type LsiDocumentCluster = Map[Int, Array[(Int, Seq[String], Int, Double)]]

  type LsiAttributeCluster = Map[Int, Array[(Int, Int,(String, Int, Int), Double)]]
}
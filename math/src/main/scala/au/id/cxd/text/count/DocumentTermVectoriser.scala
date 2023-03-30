package au.id.cxd.text.count

import au.id.cxd.text.count.TermCountTypeAliases.{TermCount, TermDocumentCount, TermHashCode}
import au.id.cxd.text.model.LatentSemanticIndex
import breeze.linalg.{DenseMatrix, DenseVector}

import scala.collection.mutable

/**
  * Created by cd on 12/1/17.
  */
trait DocumentTermVectoriser {

  /**
    * count the single query array and create a vector
    * that assigns the tfidf term weights for the query into the indices of the term matrix
    * that was constructed form the lsi model.
    * @param query
    * @param lsi
    * @return
    */
  def countQuery(query:Array[String], lsi:LatentSemanticIndex):DenseVector[Double]

  /**
    * Compute the count for the sequence of tokens found in each document.
    * Each record in the outer sequence is considered a document.
    * Each inner sequence is considered the collection of tokens within the document.
    *
    * @param data
    * @return (termIndexMap, TF-IDF Matrix)
    *
    * (mutable.Map[Int, (String, Int)], DenseMatrix[Double])
    *
    * The return is term index map that indicates which column each term is mapped to.
    * The map key contains the index of the column and the value corresponds to the term and its hashcode
    *
    * (columnIndex x (Term x Hashcode))
    *
    * The second item in the tuple is the TF-IDF matrix. Each row represents a document, each column contains the TF-IDF for the
    * corresponding term within the document.
    *
    */
  def count(data: Seq[Array[String]]): (mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)], DenseMatrix[Double])
}

package au.id.cxd.text.count

import au.id.cxd.text.count.TermCountTypeAliases.{TermCount, TermDocumentCount, TermHashCode}
import au.id.cxd.text.model.LatentSemanticIndex
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.log

import scala.collection.mutable

/**
  * ##import MathJax
  *
  * The TF IDF count will create a matrix containing tfidf counts
  * for each document.
  *
  * It will require two steps, the first it will compute the number of occurances per term in each document.
  * The second will compute the total number of occurances for each term in all documents.
  *
  * Documents are not explicitly listed by id, but their index in the supplied list is treated as the document id.
  *
  * Compute the tf-idf
  *
  * as per the wikipedia article.
  *
  * https://en.wikipedia.org/wiki/Tf–idf
  *
  * For the term frequency we use the augmented frequency to prevent bias towards longer documents
  *
  * $$
  * tf(t, d) = 0.5 + 0.5 \times \frac{f_{t,d}}{max\left(f_{t',d} : t' \in d\right)}
  * $$
  *
  * For the inversse document frequency it is calculated using the logarithm of
  *
  * $$
  * idf(t, D) = \log{\left[ \frac{N}{\left| d \in D : t \in d \right|} \right]}
  * $$
  *
  *
  * Created by cd on 7/1/17.
  */


case class TfIdfCount() extends DocumentTermVectoriser {

  /**
    * term -> document x count
    *
    */
  val termDocumentCount: mutable.Map[String, mutable.Map[Int, Double]] = mutable.Map[String, mutable.Map[Int, Double]]()


  val docMaxCount: mutable.Map[Int, Double] = mutable.Map[Int, Double]()

  /**
    * term -> count
    */
  val termCount: mutable.Map[String, Double] = mutable.Map[String, Double]()

  /**
    * check the maximum count of all terms for the supplied document.
    *
    * @param idx
    * @param cnt
    * @param docCounts
    * @return
    */
  def checkMaxCount(idx: Int, cnt: Double, docCounts: mutable.Map[Int, Double]) = {
    docCounts.get(idx) match {
      case Some(d) => d > cnt match {
        case true => docCounts
        case _ => {
          docCounts.update(idx, cnt)
          docCounts
        }
      }
      case _ => {
        docCounts.put(idx, cnt)
        docCounts
      }
    }
  }

  /**
    * count the number of occurances of the term in each document.
    *
    * @param row
    * @param idx
    * @param termDocs
    * @return
    */
  def countTermDocument(row: Array[String], idx: Int,
                        termDocs: mutable.Map[String, mutable.Map[Int, Double]],
                        docCounts: mutable.Map[Int, Double]): (mutable.Map[String, mutable.Map[Int, Double]], mutable.Map[Int, Double]) = {
    val (idxB, termDocsB, docCountsB) =
      row.foldLeft((idx, termDocs, docCounts)) {
        (accum, item) => {
          val idx1 = accum._1
          val termDocs1 = accum._2
          val docCounts1 = accum._3
          termDocs1.get(item) match {
            case Some(docMap) => {
              val (docMap1, docCounts2) = docMap.get(idx1) match {
                case Some(cnt) => {
                  docMap.update(idx1, cnt + 1.0)
                  val docCounts2 = checkMaxCount(idx1, cnt + 1.0, docCounts1)
                  (docMap, docCounts2)
                }
                case _ => {
                  docMap.put(idx1, 1.0)
                  val docCounts2 = checkMaxCount(idx1, 1.0, docCounts1)
                  (docMap, docCounts2)
                }
              }
              termDocs1.update(item, docMap1)
              (idx1, termDocs1, docCounts2)
            }
            case None => {
              termDocs1.put(item, mutable.Map(idx1 -> 1.0))
              val docCounts2 = checkMaxCount(idx1, 1.0, docCounts1)
              (idx1, termDocs1, docCounts2)
            }
          }
        }
      }
    (termDocsB, docCountsB)
  }

  /**
    * update the count of terms.
    *
    * @param row
    * @param terms
    * @return
    */
  def countTerms(row: Array[String], terms: mutable.Map[String, Double]) = {
    row.foldLeft(terms) {
      (accum, item) => {
        accum.contains(item) match {
          case true => {
            accum.update(item, accum.get(item).get + 1.0)
            accum
          }
          case _ => {
            accum.put(item, 1.0)
            accum
          }
        }
      }
    }
  }


  /**
    * update the counts and return the tuple of total and document counts
    *
    * @param row
    * @param idx
    * @param terms
    * @param termDocs
    * @return
    */
  def countTermRow(row: Array[String], idx: Int, terms: mutable.Map[String, Double],
                   termDocs: mutable.Map[String, mutable.Map[Int, Double]],
                   docCounts: mutable.Map[Int, Double]) = {
    val terms1 = countTerms(row, terms)
    val (termDocs1, docCounts1) = countTermDocument(row, idx, termDocs, docCounts)
    (terms1, termDocs1, docCounts1)
  }

  /**
    * find the matching index for the supplied term.
    *
    * @param colTermMap
    * @param term
    * @return
    */
  def findIndex(colTermMap: mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)], term: String): Option[(Int, (String, TermHashCode, TermDocumentCount, TermCount))] = {
    val matches = colTermMap.filter {
      pair => pair._2._1.equalsIgnoreCase(term)
    }
    // there should only be 1 match
    matches.size > 0 match {
      case true => {
        val item = matches.head
        Some(item)
      }
      case _ => None
    }
  }

  /**
    * count the single query array and create a vector
    * that assigns the tfidf term weights for the query into the indices of the term matrix
    * that was constructed form the lsi model.
    * @param query
    * @param lsi
    * @return
    */
  def countQuery(query:Array[String], lsi:LatentSemanticIndex):DenseVector[Double] = {
    val docIdMap = lsi.docIdMap
    val colTermMap = lsi.colTermMap

    // now we need to map the query to term indexes and create a single vector that can be projected into the search space.
    // only include the indices for known terms, if the indices are empty this means we do not have an SVD that
    // includes the terms in the query.
    val indices = query.foldLeft(List[(Int, Int)]()) {
      (accum, term) => {
        findIndex(colTermMap, term) match {
          case Some(matchItem) => {
            accum :+ (matchItem._1, matchItem._2._3)
          }
          case _ => accum
        }
      }
    }
    // build the query term vector we need to compute the tfidf for the query.
    // this preprocessing should be done by an external counter such as the TFIDF counter implementation
    val totalDocs = docIdMap.size
    // since there are different methods of constructing term counts.
    val groups = indices.groupBy(_._1)
    val maxTermCnt = indices.length > 0 match {
      case true => groups.map {
        groupItem => groupItem._2.length
      }.max
      case _ => 1.0
    }

    DenseVector.tabulate[Double](colTermMap.size) {
      i => {
        val matchIndices = indices.filter { pair => pair._1 == i }
        val localCnt = matchIndices.length.toDouble
        val docCnt = matchIndices.length > 0 match {
          case true => {
            matchIndices.head._2
          }
          case _ => 1.0
        }
        val tf = localCnt > 0 match {
          case true => 0.5 + 0.5 * (localCnt / maxTermCnt)
          case _ => 0.0
        }
        // TODO: we actually need the total number of documents containing the term.
        val idf = log(totalDocs / docCnt)
        //val idf = 1.0
        val tfidf = tf * idf
        tfidf
      }
    }
  }


  /**
    * Compute the count for the sequence of tokens found in each document.
    * Each record in the outer sequence is considered a document.
    * Each inner sequence is considered the collection of tokens within the document.
    *
    * @param data
    * @return (termIndexMap, TF-IDF Matrix)
    *
    * (mutable.Map[Int, (String, Int, Int)], DenseMatrix[Double])
    *
    * The return is term index map that indicates which column each term is mapped to.
    * The map key contains the index of the column and the value corresponds to the term and its hashcode
    *
    * (columnIndex x (Term x Hashcode x Number of Documents containing term))
    *
    * The second item in the tuple is the TF-IDF matrix. Each row represents a document, each column contains the TF-IDF for the
    * corresponding term within the document.
    *
    */
  def count(data: Seq[Array[String]]): (mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)], DenseMatrix[Double]) = {

    val counts = data.foldLeft((0, termCount, termDocumentCount, docMaxCount)) {
      (accum, row) => {
        val idx = accum._1
        val terms = accum._2
        val termDocs = accum._3
        val docCounts = accum._4
        val (terms1, termDocs1, docCounts1) = countTermRow(row, idx, terms, termDocs, docCounts)
        (idx + 1, terms1, termDocs1, docCounts1)
      }
    }
    val terms = counts._2
    val termDocs = counts._3
    val docCounts = counts._4

    // we need to generate a map from
    val hashes = terms.keys.map { key => key.hashCode }
    val keys = terms.keys.zip(hashes).toSeq
    // we have a map of hashes now we need to sort the keys by hash
    val keySorted = keys.sortBy(pair => pair._2)
    // the last value in the tuple is the count of documents in our data set that contains the term.
    // keyMap is (termColumn x (Term, HashCode, docCount, TermCount))
    val keyMap = mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)]()
    for (i <- 0 until keys.length) {
      val pair = keySorted(i)
      val ndocs = termDocs.get(pair._1).get.keys.size
      val termCount = terms.get(pair._1).getOrElse(0.0)
      keyMap.put(i, (pair._1, pair._2, ndocs, termCount.toInt))
    }
    // now there is a key map which we use to index the columns of each document when calculating the tf-idf
    val tfIdfMat = DenseMatrix.tabulate[Double](data.length, keys.length) {
      (i, j) =>

        /**
          * Compute the tf-idf
          *
          * as per the wikipedia article.
          *
          * https://en.wikipedia.org/wiki/Tf–idf
          *
          * For the term frequency we use the augmented frequency to prevent bias towards longer documents
          *
          * $$
          * tf(t, d) = 0.5 + 0.5 \times \frac{f_{t,d}}{max\left(f_{t',d} : t' \in d\right)}
          * $$
          *
          * For the inversse document frequency it is calculated using the logarithm of
          *
          * $$
          * idf(t, D) = \log{\left[ \frac{N}{\left| d \in D : t \in d \right|} \right]}
          * $$
          */
        val termMap = keyMap.get(j).get
        // number of documents the term appears in
        val ndocs = termDocs.get(termMap._1).get.keys.size
        val docCnt = termDocs.get(termMap._1).get.find { pair => pair._1 == i
        } match {
          case Some(pair) => pair._2
          case _ => 0.0
        }
        val maxDocCount = docCounts.get(i) match {
          case Some(n) => n
          case _ => 1.0
        }
        val idf = log(data.length / ndocs)
        // adjust for maximum frequency of terms in the document at index i
        val tf = 0.5 + 0.5 * (docCnt / maxDocCount)
        val tfidf = tf * idf
        tfidf
    }

    // we have the terms ordered by their keys and the TFIDF matrix so we return both.
    (keyMap, tfIdfMat)
  }

}

package au.id.cxd.text.model

import breeze.linalg.svd.{DenseSVD, SVD}
import breeze.linalg.{DenseMatrix, DenseVector}

import java.nio.file.{Files, Path, Paths}
import java.io.File
import breeze.linalg._
import au.id.cxd.math.data.{CsvReader, CsvWriter, Readable, Writable}
import au.id.cxd.math.data.archive.{ZipArchiveInput, ZipArchiveOutput}
import au.id.cxd.math.function.distance.CosineDistance
import au.id.cxd.math.model.components.SingularValueDecomposition
import au.id.cxd.text.count.TermCountTypeAliases.{TermCount, TermDocumentCount, TermHashCode}
import au.id.cxd.text.count.{DocumentTermVectoriser, TfIdfCount}
import au.id.cxd.text.helpers.{EmbeddedStopwordsLoader, IndexedTextCsvReader}
import au.id.cxd.text.preprocess.{LinePatternFilter, StemmingPatternFilter, StopwordPatternFilter}

import scala.collection.mutable
import scala.util.Try

/**
  * The latent semantic index is a class that contains the data associated with
  * the latent semantic index that is constructed from an indexing pipeline.
  *
  * This type will have a companion object that can read and write the associated data to and from a zip archive.
  *
  * a TF-IDF document term matrix
  * a term column mapping
  * a document id x row mapping
  * and the SVD decomposition of the document.
  *
  * Created by cd on 10/1/17.
  */
class LatentSemanticIndex(val docIdMap: mutable.Map[Int, Seq[String]],
                          val colTermMap: mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)],
                          val tfIdf: DenseMatrix[Double],
                          val svD: SVD[DenseMatrix[Double], DenseVector[Double]]) extends Serializable {
}


object LatentSemanticIndex
  extends LatentSemanticIndexWriter
    with LatentSemanticIndexReader
    with LatentSemanticIndexBuilder
    with LsiDocumentSearch {




  type LsiSearchSpace = (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double])

  def apply(docIdMap: mutable.Map[Int, Seq[String]],
            colTermMap: mutable.Map[Int,  (String, TermHashCode, TermDocumentCount, TermCount)],
            tfIdf: DenseMatrix[Double],
            svD: SVD[DenseMatrix[Double], DenseVector[Double]]) = new LatentSemanticIndex(docIdMap, colTermMap, tfIdf, svD)

  def unapply(lsi: LatentSemanticIndex) = (lsi.docIdMap, lsi.colTermMap, lsi.tfIdf, lsi.svD)

}

/**
  * a writer trait for the latent semantic index data set.
  */
trait LatentSemanticIndexWriter {

  val docMapWriter = new CsvWriter(writeHeaders = true) {

    def headers(example: Seq[String]) = {
      val ids = example.zip(for (i <- 0 until example.length) yield i).map { pair => s"id${pair._2}" }
      Seq[String]("RowIndex") ++ ids
    }

    def writeBlock(pair: (Int, Seq[String])): Array[String] = {
      val n = pair._1.toString
      val end = pair._2.toArray
      Array(n) ++ end
    }
  }

  val termMapWriter = new CsvWriter(writeHeaders = true) {

    val headers = Seq("Index", "Term", "Hashcode", "DocCount", "TermCount")

    def writeBlock(pair: (Int,  (String, TermHashCode, TermDocumentCount, TermCount))): Array[String] = {
      Array(pair._1.toString, pair._2._1, pair._2._2.toString, pair._2._3.toString)
    }
  }


  /**
    * write the document Id Map
    *
    * @param path
    * @param docIdMap
    * @return
    */
  def writeDocIdMap(path: String, docIdMap: mutable.Map[Int, Seq[String]]) = {
    docMapWriter.write(path, docMapWriter.headers(docIdMap.head._2).toArray, docIdMap.toSeq)(docMapWriter.writeBlock)
  }


  def writeTermMap(path: String, termMap: mutable.Map[Int,  (String, TermHashCode, TermDocumentCount, TermCount)]) = {
    termMapWriter.write(path, termMapWriter.headers.toArray, termMap.toSeq)(termMapWriter.writeBlock)
  }

  def writeZip(index: LatentSemanticIndex)(workingPath: String)(targetPath: String) = Try {
    val parent = workingPath.stripSuffix(File.separator)
    val docPath = s"$parent${File.separator}docidmap.csv"
    val termPath = s"$parent${File.separator}termmap.csv"
    val tfidfPath = s"$parent${File.separator}tdidf-matrix.csv"
    writeDocIdMap(docPath, index.docIdMap)
    writeTermMap(termPath, index.colTermMap)
    csvwrite(new File(tfidfPath), index.tfIdf)

    // the svd components
    val leftPath = s"$parent${File.separator}leftComponentsU.csv"
    val singularPath = s"$parent${File.separator}singularValueS.csv"
    val rightPath = s"$parent${File.separator}rightComponentsVt.csv"
    csvwrite(new File(leftPath), index.svD.U)
    csvwrite(new File(singularPath), index.svD.S.toDenseMatrix)
    csvwrite(new File(rightPath), index.svD.Vt)

    // now we write into the Zip Archive

    val result = ZipArchiveOutput.
      startArchive(targetPath).
      flatMap {
        zos =>
          ZipArchiveOutput.addFileFrom(zos)(
            Seq[File](new File(docPath),
              new File(termPath),
              new File(tfidfPath),
              new File(leftPath),
              new File(singularPath),
              new File(rightPath)
            ))
      }.flatMap {
      zos => ZipArchiveOutput.close(zos)
    }

    result match {
      case Some(flag) => flag
      case _ => false
    }
  }

  /**
    * write the latent semantic index structure to the supplied path.
    *
    * @param index
    * @param path
    */
  def writeZipTemp(index: LatentSemanticIndex)(path: String) = {
    val tmpFile = path.stripSuffix(".zip")
    val p1 = Paths.get(tmpFile)
    val cnt = p1.getNameCount()
    val childDir = p1.getName(cnt-1)
    val tmp = Files.createTempDirectory(childDir.toString)
    tmp.toFile.deleteOnExit()
    val tmpPath = tmp.toString
    // now we can create a set of separate files.
    val result = writeZip(index)(tmpPath)(path)
    tmp.toFile.delete()
    result
  }

  /**
    * write the model in binary format instead of a zip archive.
    *
    * @param lsi
    * @param path
    */
  def writeBinary(lsi: LatentSemanticIndex)(path: String) = {
    val writer = new Writable[LatentSemanticIndex] {}
    writer.write(path)(lsi)
  }


}

/**
  * a reader trait for the latent semantic index.
  */
trait LatentSemanticIndexReader {


  /**
    * read the document index map
    *
    * @param path
    * @return
    */
  def readDocMap(path: String): mutable.Map[Int, Seq[String]] = {
    val (idx, doc) =
      CsvReader().readCsv(new File(path), (-1, mutable.Map[Int, Seq[String]]())) {
        (accum, line) => {
          val idx = accum._1
          idx < 0 match {
            case true => {
              (0, accum._2)
            }
            case _ => {
              val rowId = line.head
              val docIds = line.tail
              accum._2.put(rowId.toInt, docIds.toSeq)
              (idx + 1, accum._2)
            }
          }

        }
      }
    doc
  }


  /**
    * read the column term map from CSV.
    *
    * @param path
    * @return
    */
  def readColTermMap(path: String): mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)] = {
    val (idx, terms) =
      CsvReader().readCsv(new File(path), (-1, mutable.Map[Int, (String, TermHashCode, TermDocumentCount, TermCount)]())) {
        (accum, line) => {
          val idx = accum._1
          idx < 0 match {
            case true => (0, accum._2)
            case _ => {
              val colIdx = line(0).toInt
              val term = line(1)
              val hashCode = line(2).toInt
              val docCnt = line(3).toInt
              val termCount = line(4).toDouble
              accum._2.put(colIdx, (term, hashCode, docCnt, termCount))
              (idx + 1, accum._2)
            }
          }
        }
      }
    terms
  }

  /**
    * read the SVD from the supplied path.
    *
    * @param zipFile
    * @param path
    * @return
    */
  def extractAndReadFromPath(zipFile: String)(path: String) = Try {

    val zis = ZipArchiveInput.openArchive(zipFile).flatMap {
      zis => ZipArchiveInput.extractToPath(zis)(path)
    }
    val parent = path.stripSuffix(File.separator)
    // the set of files should exist beneath the target path
    val docPath = s"$parent${File.separator}docidmap.csv"
    val termPath = s"$parent${File.separator}termmap.csv"
    val tfidfPath = s"$parent${File.separator}tdidf-matrix.csv"

    val leftPath = s"$parent${File.separator}leftComponentsU.csv"
    val singularPath = s"$parent${File.separator}singularValueS.csv"
    val rightPath = s"$parent${File.separator}rightComponentsVt.csv"


    val docMap = readDocMap(docPath)
    val terms = readColTermMap(termPath)
    val tfIdfM = csvread(new File(tfidfPath))

    val leftU = csvread(new File(leftPath))
    val singularM = csvread(new File(singularPath)).toDenseVector
    val rightVt = csvread(new File(rightPath))

    new LatentSemanticIndex(docMap, terms, tfIdfM, SVD(leftU, singularM, rightVt))

  }

  /**
    * read from the zip archive.
    * @param zipFile
    * @return
    */
  def readZip(zipFile: String) = {
    // read the zip file into a temporary directory and convert it into a data set.
    val tmpPath = Files.createTempDirectory("lsizip")
    tmpPath.toFile.deleteOnExit()
    val result = extractAndReadFromPath(zipFile)(tmpPath.toString)
    tmpPath.toFile.delete()
    result
  }

  /**
    * read from binary
    * @param filePath
    * @return
    */
  def readBinary(filePath:String) = {
    val reader = new Readable[LatentSemanticIndex] {}
    reader.read(filePath)
  }
}

/**
  * the pipeline that is used to build the index.
  *
  * - build the tfidf matrix
  * - perform the SVD
  * - capture the entropy in the data set and the component contribution for each singular component.
  * -
  */
trait LatentSemanticIndexBuilder {

  /**
    * load the default set of stopwords using the embedded stopwords loader.
    *
    * @return
    */
  def loadStopWords() = {
    val loader = EmbeddedStopwordsLoader()
    val stopwords = loader.load()
    stopwords
  }

  /**
    * read an indexed CSV file containing document Ids for each row and string text for each document.
    *
    * @param file
    * @param docIdCols
    * @param skipHeader
    * @return
    */
  def readIndexedCsv(path: String, docIdCols: Seq[Int], skipHeader: Boolean = true) = {
    val file = new File(path)
    val (docMap, dataSet) = IndexedTextCsvReader(skipHeader = true, idColumns = docIdCols).readAndIndexCsv(file)
    (docMap, dataSet.map {
      _.head
    })
  }


  /**
    * extract stemmed terms from the supplied sequence of lines
    *
    * @param stopwords
    * @param lines
    * @return
    */
  def extractStemmedTerms(stopwords: Seq[String], lines: Seq[String]) = {
    val stemmer = new StemmingPatternFilter(stopwords)
    val stemmedTerms = stemmer.filter(lines)
    stemmedTerms
  }

  /**
    * extract terms without stemming.
    *
    * @param stopwords
    * @param lines
    */
  def extractTerms(stopwords: Seq[String], lines: Seq[String]) = {
    val termFilter = new StopwordPatternFilter(stopwords)
    val terms = termFilter.filter(lines)
    terms
  }

  /**
    * compute the TfIdf matrix
    *
    * @param terms
    * @return
    */
  def computeTfIdf(terms: Seq[Array[String]]) = {
    val tfIdfCounter = TfIdfCount()
    // the columnMap has (columnIdx, (Term, HashCode))
    // the TfIdf is the matrix containing calculated weights for each document.
    val (columnMap, tfIdf) = tfIdfCounter.count(terms)
    (columnMap, tfIdf)
  }

  /**
    * compute the Svd along with the entropy of the data set and the contributions of each singular value component.
    *
    * @param tfIdf
    * @return
    */
  def computeSvd(tfIdf: DenseMatrix[Double]): (DenseSVD, Double, DenseVector[Double]) = {
    val svD = SingularValueDecomposition(tfIdf)
    val entropy = SingularValueDecomposition.entropy(svD)
    val contributions = SingularValueDecomposition.contributions(svD)
    (svD, entropy, contributions)
  }

  /**
    * recompute the entire latent semantic index
    * from the data stored in the supplied CSV file.
    * The CSV file must include one or more document ids for each row and must include the document text content in the remaining column.
    *
    * Eg:
    * DocID1,SubID2,Text
    * 1,     111   ,   "This is some text data"
    *
    * The fields should be quoted either with " or '.
    *
    * @param inputCsv
    * @param docIdCols
    * @param skipHeader
    * @param stemTerms
    * @return
    *
    * The return result includes the latent semantic index, along with the entropy of the terms calculated from the data set
    * and the contributions of each singular component to the total entropy of the document set.
    *
    */
  def buildFromCsv(inputCsv: String, docIdCols: Seq[Int], skipHeader: Boolean = true, stemTerms: Boolean = true) = {
    val (docMap, lines) = readIndexedCsv(inputCsv, docIdCols, skipHeader)
    val stopwords = loadStopWords()
    val terms = stemTerms match {
      case true => extractStemmedTerms(stopwords, lines.toSeq)
      case _ => extractTerms(stopwords, lines.toSeq)
    }
    val tfIdfCounter = TfIdfCount()
    // the columnMap has (columnIdx, (Term, HashCode))
    // the TfIdf is the matrix containing calculated weights for each document.
    val (columnMap, tfIdf) = tfIdfCounter.count(terms)
    // we have the local copy of the Svd.
    val (svD, entropy, contributions) = computeSvd(tfIdf)
    // create the LSI instance
    val lsi = new LatentSemanticIndex(docMap, columnMap, tfIdf, svD)
    (entropy, contributions, lsi)
  }


}

/**
  * implementation of document search for supplied input query array.
  * Note that the input query should contain terms that have previously existed within the LSI model.
  * The LSI model contains a term map, the set of terms not found in the vocabulary are also returned with the result.
  * The cosine distance is returned unnormalised.
  */
trait LsiDocumentSearch {


  /**
    * convert the query into a term vector.
    *
    * @param query
    * @param stopwords
    * @param lsi : LatentSemanticIndex
    * @param stemQuery
    * @return
    * the query term vector.
    * currently the term vector is a tfidf vector for the query based on the lsi model.
    * However there are other methods of weighting terms so it will be changed
    * to supply a counting trait to calculate the term weights
    */
  def preprocessQuery(vectoriser: DocumentTermVectoriser)(query: Array[String], stopwords: Seq[String], lsi: LatentSemanticIndex, stemQuery: Boolean = true) = {

    val query1 = stemQuery match {
      case true =>
        new StemmingPatternFilter(stopwords).tokeniseQuery(query)
      case _ =>
        new StopwordPatternFilter(stopwords).tokeniseQuery(query)
    }
    vectoriser.countQuery(query1, lsi)
  }


  /**
    * The singular values can dictate how many dimensions we should retain.
    * After manual analysis it may be decided that we only want to retain a certain fixed number of dimensions
    *
    * Hence this will result in an SVD of reduced dimensionality where k is the number of principle components.
    *
    * The original SVD
    *
    * $$
    * \hat{X} = U S V'
    * $$
    *
    * where $U$ has dimension $(m x n)$
    * and $S$ has size $n$
    * and $Vt$ has dimension $(n x n)$
    *
    * If we choose dimension $k < n$ then we have
    *
    * $U$ dimension $(m x k)$
    * $S$ dimension $k$
    * $V'$ dimension $(k x n)$
    *
    * Note that this does not reduce the original tfidf matrix, but will reduce the dimensions that the projects of the tfidf into the search space will have.
    *
    * Note that $k < n$
    *
    * @param k
    */
  def reduceToDimensons(lsi: LatentSemanticIndex, k: Int): LatentSemanticIndex = {
    val U = lsi.svD.U
    val S = lsi.svD.S
    val Vt = lsi.svD.Vt

    val rU = U(::, 0 until k).toDenseMatrix
    val rS = S(0 until k).toDenseVector
    val rVt = Vt(0 until k, ::).toDenseMatrix

    LatentSemanticIndex(lsi.docIdMap, lsi.colTermMap, lsi.tfIdf, SVD(rU, rS, rVt))
  }

  /**
    * generate the search space for the U and V components by multiplying them against the
    * square root of the singular value diagonal matrix.
    *
    * Note the search space may need to be cached
    *
    * @param lsi
    */
  def makeSearchSpace(lsi: LatentSemanticIndex) = {
    val searchSI = SingularValueDecomposition.singularRootDiagonal(lsi.svD)
    val searchU = lsi.svD.U * searchSI
    val searchVt = lsi.svD.Vt.t * searchSI
    (searchU, searchSI, searchVt)
  }

  /**
    * search in the lsi model with an array query.
    *
    * @param searchSpace
    * @param vectoriser
    * @param query
    * @param stopWords
    * @param lsi
    * @param stemQuery
    * @return
    */
  def performSearch(searchSpace: (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]),
                    vectoriser: DocumentTermVectoriser,
                    query: Array[String],
                    stopWords: Seq[String],
                    lsi: LatentSemanticIndex,
                    stemQuery: Boolean = true): mutable.Buffer[(Int, Double, Seq[String])] = {
    val ssU = searchSpace._1
    val ssVt = searchSpace._3
    val queryVect = preprocessQuery(vectoriser)(query, stopWords, lsi, stemQuery)
    val queryProj = queryVect.toDenseMatrix * ssVt
    // the query projection should be a 1 x k matrix depending on the k components that have been kept for the svd.
    val distances = CosineDistance(ssU, queryProj.toDenseVector)
    // we now have distances that correspond to document indexes, zip the distances and the document index
    // sort by reverse order, since distances closer to 1 are more closely aligned in vector space, distances close to 0 are close to perpendicular.
    lsi.docIdMap.map {
      entry => {
        val idx = entry._1
        // note the same number of rows exist in the distances vector
        // we use the index of the document map to look it up.
        // we cannot assume the document map indexes are ordered at all.
        val dist = distances(idx)
        (idx, dist, entry._2)
      }
    }.toBuffer
      .sortBy { pair => -pair._2 }
  }

}
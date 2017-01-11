package au.id.cxd.text.model

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import java.nio.file.{Files, Path}
import java.io.File

import breeze.linalg._
import au.id.cxd.math.data.{CsvReader, CsvWriter}
import au.id.cxd.math.data.archive.{ZipArchiveInput, ZipArchiveOutput}
import au.id.cxd.math.model.components.SingularValueDecomposition
import au.id.cxd.text.count.TfIdfCount
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
                          val colTermMap: mutable.Map[Int, (String, Int)],
                          val tfIdf: DenseMatrix[Double],
                          val svD: SVD[DenseMatrix[Double], DenseVector[Double]]) {


}


object LatentSemanticIndex
  extends LatentSemanticIndexWriter
    with LatentSemanticIndexReader
    with LatentSemanticIndexBuilder {

  def apply(docIdMap: mutable.Map[Int, Seq[String]],
            colTermMap: mutable.Map[Int, (String, Int)],
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

    val headers = Seq("Index", "Term", "Hashcode")

    def writeBlock(pair: (Int, (String, Int))): Array[String] = {
      Array(pair._1.toString, pair._2._1, pair._2._2.toString)
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


  def writeTermMap(path: String, termMap: mutable.Map[Int, (String, Int)]) = {
    termMapWriter.write(path, termMapWriter.headers.toArray, termMap.toSeq)(termMapWriter.writeBlock)
  }

  def write(index: LatentSemanticIndex)(workingPath: String)(targetPath: String) = Try {
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
  def writeTemp(index: LatentSemanticIndex)(path: String) = {
    val tmpFile = path.stripSuffix(".zip")
    val tmp = Files.createTempDirectory(tmpFile)
    tmp.toFile.deleteOnExit()
    val tmpPath = tmp.toString
    // now we can create a set of separate files.
    write(index)(tmpPath)(path)
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
              accum._2.put(rowId.toInt, docIds)
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
  def readColTermMap(path: String): mutable.Map[Int, (String, Int)] = {
    val (idx, terms) =
      CsvReader().readCsv(new File(path), (-1, mutable.Map[Int, (String, Int)]())) {
        (accum, line) => {
          val idx = accum._1
          idx < 0 match {
            case true => (0, accum._2)
            case _ => {
              val colIdx = line(0).toInt
              val term = line(1)
              val hashCode = line(2).toInt
              accum._2.put(colIdx, (term, hashCode))
              (idx + 1, accum._2)
            }
          }
          accum
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
  def extractAndReadFromPath(zipFile: String)(path: String) = {

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

  def readZip(zipFile: String) = {
    // read the zip file into a temporary directory and convert it into a data set.
    val tmpPath = Files.createTempDirectory("lsizip")
    tmpPath.toFile.deleteOnExit()
    extractAndReadFromPath(zipFile)(tmpPath.toString)
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
  def computeSvd(tfIdf: DenseMatrix[Double]) = {
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
    val (termMap, terms) = stemTerms match {
      case true => extractStemmedTerms(stopwords, lines)
      case _ => extractTerms(stopwords, lines)
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

  def preprocessQuery(query:Array[String], stopwords:Seq[String], colTermMap:mutable.Map[Int, (String, Int)], stemQuery:Boolean = true) = {
    val query1 = stemQuery match {
      case true =>
        new StemmingPatternFilter(stopwords).tokeniseQuery(query)
      case _ =>
        new StopwordPatternFilter(stopwords).tokeniseQuery(query)
    }
    // now we need to map the query to term indexes and create a single vector that can be projected into the search space.

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
    val searchVt = lsi.svD.Vt * searchSI
    (searchU, searchSI, searchVt)
  }



}
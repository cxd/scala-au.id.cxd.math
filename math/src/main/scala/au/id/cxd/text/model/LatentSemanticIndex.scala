package au.id.cxd.text.model

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import java.nio.file.{Files, Path}
import java.io.File

import breeze.linalg._
import au.id.cxd.math.data.CsvWriter
import au.id.cxd.math.data.archive.ZipArchiveOutput

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

object LatentSemanticIndex {

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
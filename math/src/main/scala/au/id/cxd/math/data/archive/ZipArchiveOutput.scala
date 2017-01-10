package au.id.cxd.math.data.archive

import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.util.Try

/**
  * A set of functions for working with ZipArchives
  * Created by cd on 10/1/17.
  */
object ZipArchiveOutput {

  /**
    * start writing an archive
    *
    * @param path
    * @return
    */
  def startArchive(path: String) = Try {
    val fos = new FileOutputStream(path)
    val zos = new ZipOutputStream(new BufferedOutputStream(fos))
    zos
  } toOption

  /**
    * add a file to the archive
    *
    * @param zos
    * @param fileName
    * @param data
    */
  def addFile(zos: ZipOutputStream)(fileName: String, data: Array[Byte]) = Try {
    val entry = new ZipEntry(fileName)
    zos.putNextEntry(entry)
    zos.write(data)
    zos.flush()
    zos
  } toOption

  /**
    * add a sequence of files to the archive
    * @param zos
    * @param pairs
    * @return
    */
  def addFiles(zos:ZipOutputStream)(pairs:Seq[(String,Array[Byte])]) = Try {
    pairs.foreach {
      pair => {
        val fileName = pair._1
        val bytes = pair._2
        val entry = new ZipEntry(fileName)
        zos.putNextEntry(entry)
        zos.write(bytes)
        zos.flush()
      }
    }
    zos
  } toOption

  /**
    * add a sequence of files from disc
    * @param zos
    * @param files
    * @return
    */
  def addFileFrom(zos:ZipOutputStream)(files:Seq[File]) = Try {
    val bufSize = 2048
    files.foreach {
        file => {
          val filename = file.getName
          val is = new FileInputStream(file)
          val in = new BufferedInputStream(is)
          val bytes = Iterator.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
          val entry = new ZipEntry(filename)
          zos.putNextEntry(entry)
          zos.write(bytes)
          zos.flush()
          in.close()
        }
      }
    zos
  } toOption

  /**
    * close the archive
    * @param zos
    * @return
    */
  def close(zos:ZipOutputStream) = Try {
    zos.flush()
    zos.close()
    true
  } toOption




}

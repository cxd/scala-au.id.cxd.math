package au.id.cxd.math.data.archive

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.ZipInputStream

import scala.collection.mutable
import scala.util.Try

/**
  * Extract a zip archive either into memory or into a target directory.
  * Created by cd on 10/1/17.
  */
object ZipArchiveInput {


  /**
    * extract the input zip file to the output path.
    * @param file
    * @param outputPath
    * @return
    */
  def extract(file:String, outputPath:String) = {
    openArchive (file).flatMap{
      zis => extractToPath(zis)(outputPath)
    }.flatMap (closeArchive) match {
      case Some(flag) => flag
      case _ => false
    }
  }

  /**
    * open an archive
    * @param file
    * @return
    */
  def openArchive(file:String) = Try {
    val is = new FileInputStream(file)
    val zis = new ZipInputStream(is)
    zis
  } toOption

  /**
    * extract the array in memory into a sequence of
    * name, byte array pairs.
    * @param zis
    * @return
    */
  def extractInMemory(zis:ZipInputStream) = Try {
    // note very idiomatic either, however due to the iterator for zis entries
    val data = mutable.ListBuffer[(String, Array[Byte])]()
    var entry = zis.getNextEntry
    while(entry != null) {
      val name = entry.getName
      val bytes = Iterator.continually(zis.read()).takeWhile(-1 !=).map(_.toByte).toArray
      data :+ (name, bytes)
      entry = zis.getNextEntry
    }
    (zis, data)
  } toOption

  /**
    * extract the files to the base path supplied
    * @param path
    * @return
    */
  def extractToPath(zis:ZipInputStream)(path:String) = Try {
    // ensure the output path exists, create it otherwise.
    val outFile = new File(path)
    outFile.exists() match {
      case false => outFile.mkdir()
      case _ => ()
    }

    // unfortunately this is imperative couldn't work out how to make this operate with Iterator.
    var entry = zis.getNextEntry
    while(entry != null) {
      val subpath = path.stripSuffix(File.separator)
      val name = entry.getName
      val targetFile = s"${path}${File.separator}$name"
      val fos = new FileOutputStream(targetFile)
      val bytes = zis.available()

      val data:Array[Byte] = Iterator.continually(zis.read()).takeWhile(-1 != ).map(_.toByte).toArray
      fos.write(data)
      fos.flush()
      fos.close()
      entry = zis.getNextEntry
    }
    zis
  } toOption

  /**
    * close the archive.
    * @param zis
    * @return
    */
  def closeArchive(zis:ZipInputStream) = Try {
    zis.close()
    true
  } toOption

}

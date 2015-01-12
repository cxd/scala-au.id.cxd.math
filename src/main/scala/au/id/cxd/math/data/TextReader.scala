package au.id.cxd.math.data

import java.io.{FileInputStream, InputStream, File}

import scala.io.BufferedSource

/**
 * Created by cd on 10/01/15.
 */
class TextReader {

  /**
   * read all lines in a supplied file
   * @param file
   * @return
   */
  def readLines (file:File) : List[String] = {
    val inputStream = new FileInputStream(file)
    val buffer = new BufferedSource(inputStream)
    val result = List[String]()
    val lines = buffer.getLines().foldLeft (result) {
      (accum:List[String], item:String) => {
        accum ::: List(item)
      }
    }
    buffer.close()
    lines
  }

  /**
   * skip all lines that are either empty or start with the hash # token
   * @param lines
   * @return
   */
  def removeComments (lines:List[String]) : List[String] = {
    lines.filter {
      (line) => !line.isEmpty && !line.startsWith("#")
    }
  }

}

object TextReader {
  def apply() = new TextReader()
}

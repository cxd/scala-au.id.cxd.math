package au.id.cxd.math.data

import java.io.{File, FileInputStream, InputStream}

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

/**
  * A text reader class that consumes a line delimited file.
  *
  * Created by cd on 10/01/15.
  */
class TextReader {

  /**
    * read all lines in a supplied file
    *
    * @param file
    * @return
    */
  def readLines(file: File): ListBuffer[String] = {
    val inputStream = new FileInputStream(file)
    val buffer = new BufferedSource(inputStream)
    val result = ListBuffer[String]()
    result.appendAll(buffer.getLines())
    buffer.close()
    result
  }

  /**
    * match comments prefixed with #
    *
    * @param line
    * @return
    */
  def isComment(line: String): Boolean =
    line.isEmpty || line.startsWith("#")


  /**
    * skip all lines that are either empty or start with the hash # token
    *
    * @param lines
    * @return
    */
  def removeComments(lines: List[String]): List[String] =
    lines.filter { line => !isComment(line) }


}

object TextReader {
  def apply() = new TextReader()
}

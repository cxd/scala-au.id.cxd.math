package au.id.cxd.math.data

import java.io.{BufferedOutputStream, FileOutputStream, FileWriter, PrintWriter}

import scala.util.Try

/**
  * Created by cd on 10/1/17.
  */
class CsvWriter(val writeHeaders:Boolean, val separator:String=",", val encloseChar:String="") {

  /**
    * format the line as CSV sequence.
    * Determine whether to encapsulate fields.
    * @param row
    * @return
    */
  def format(row:Array[String]):String = {
    encloseChar.isEmpty match {
      case true => row.mkString(separator)
      case _ => row.map { r => s"""$encloseChar$r$encloseChar"""}.mkString(separator)
    }
  }

  /**
    * write the data type T to CSV
    * @param targetFile
    * @param headers
    * @param data
    * @param block
    * @tparam T
    * @return
    */
  def write[T](targetFile:String, headers:Array[String], data:Seq[T])(block: T => Array[String]) = Try {
    val fout = new FileWriter(targetFile)
    val os = new PrintWriter(fout)
    writeHeaders match {
      case true => {
        val line = format(headers)
        os.println(line)
      }
      case _ => {}
    }
    data.foreach {
      datum => {
        val row = block(datum)
        val line = format(row)
        os.println(line)
      }
    }
    os.flush()
    os.close()
    true
  }

}

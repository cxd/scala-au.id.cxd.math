package au.id.cxd.math.data

import java.io.{ObjectInputStream, FileInputStream}

import scala.util.Try

/**
 * Created by cd on 10/02/15.
 */
trait Readable[T] {

  /**
   * read from file and return as T
   * @param file
   * @return
   */
  def read(file:String):Option[T] = {
    Try {
      val infile = new FileInputStream(file)
      val os = new ObjectInputStream(infile)
      val result = os.readObject().asInstanceOf[T]
      os.close()
      result
    }.toOption
  }

}

package au.id.cxd.math.data

import scala.util.Try
import scala.util.control.Exception._
import java.io.{ObjectOutputStream, FileOutputStream}

/**
 * Created by cd on 10/02/15.
 */
trait Writable[T] {

  /**
   * write data instance to file.
   * @param file
   * @param data
   * @return
   */
  def write(file:String)(data:T) : Option[Boolean] = {
    Try {
      val out = new FileOutputStream(file)
      val os = new ObjectOutputStream(out)
      os.writeObject(data)
      os.flush()
      os.close()
      true
    } toOption
  }

}

package data

import java.io.File

import au.id.cxd.math.data.SequenceReader
import org.scalatest.{FlatSpec, Matchers}

/**
 *
 * Test reading sequences from csv file
 *
 * Created by cd on 12/01/15.
 */
class TestSequenceReader extends FlatSpec with Matchers {

  val fileName = "example_train_data.csv"

  "reader" should "read sequences" in {
    val url = getClass.getClassLoader.getResource(fileName)
    val file = new File(url.getFile)
    println("File Path: " + file.getAbsolutePath)
    val reader = SequenceReader()
    val data = reader.readSequences(file)
    println(data)
    data.length > 0 should be (true)
  }

}

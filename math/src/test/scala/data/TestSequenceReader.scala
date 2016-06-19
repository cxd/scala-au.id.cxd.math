package data

import java.io.File

import au.id.cxd.math.data.SequenceReader
import org.scalatest.{FlatSpec, _}

/**
 *
 * Test reading sequences from csv file
 *
 * Created by cd on 12/01/15.
 */
class TestSequenceReader extends FlatSpec with ShouldMatchers {

  val fileName = "src/test/data/example_train_data.csv"

  "reader" should "read sequences" in {
    val file = new File(fileName)
    println("File Path: " + file.getAbsolutePath)
    val reader = SequenceReader()
    val data = reader.readSequences(file)
    println(data)
    data.length > 0 should be (true)
  }

}

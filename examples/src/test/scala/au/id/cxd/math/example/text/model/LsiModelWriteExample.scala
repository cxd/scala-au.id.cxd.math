package au.id.cxd.math.example.text.model

import java.io.File

import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.example.text.model.LsiModelExample.getClass
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.LatentSemanticIndex

import scala.util.Success

/**
  * This example demonstrates how to build the lsi model and then write it to a zip archive using the built in methods.
  *
  * The model contains the SVD and associated term column map and document index map.
  *
  *
  * The process to build the SVD may take up to over 3gb of RAM once completed it will then free memory back around 1gb.
  * This is due to the moderate data size. Obviously depending on the volume of data, RAM and CPU will affect performance.
  * The algorithm is not a distributed algorithm and computes in process.
  *
  * Additionally writing the data to disc may take several gb of memory, and as the csv output is not optimised this is the main bottleneck of the process.
  * However CSV is used as it can be loaded in R and is convenient for use in R for visualisation.
  *
  * For this example make sure to add java arguments for -Xmx6g -Xms1g
  *
  *
  * Note that it is possible to serialize the data in other formats and simply construct an instance of the LatentSemanticIndex
  * refer to the object LatentSemanticIndexWriter for example of how to customise writing to alternate sources.
  * In the default case it uses CSVs which are archived in a zip file.
  *
  * The object LatentSemanticIndexReader provides an example of reading from the zip archive, but similarly a
  * complementary approach to reading custom serialization could also be achieved should that be preferred in application.
  *
  *
  * Created by cd on 13/1/17.
  */
object LsiModelWriteExample {

  val textData = "example_text_corpus_data.csv"

  // binary serialization file
  val targetSer = "lsiexample.ser"
  // zip archives with CSV for use in other tools for visualisation
  val targetZip = "lsiexample.zip"

  def buildModel() = {
    val url = getClass.getClassLoader().getResource(textData)
    val inputCsv = url.getFile
    val idCols = Seq(0,1)
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols)

    println(s"Entropy: $entropy")
    println(s"Contributions: $contributions")
    // TODO: plot contributions for first k

    println(s"Dimensions: U (${lsi.svD.U.rows} x ${lsi.svD.U.cols}) S: ${lsi.svD.S.length} Vt: (${lsi.svD.Vt.rows} x ${lsi.svD.Vt.cols})")

    // Now given that the entropy is very close to 0 this means the variation is explained largely in the first dimension.
    // however the first component explains the 99% or so of the variation.
    // we will reduce the LSI to roughly 1000 dimensions which is less than the 5000 or so dimensions from the original tfidf.
    // typically after building the initial SVD, some inspection is required to determine how many dimensions should be kept.
    // hence a separate exploration example should be shown.
    val lsi2 = LatentSemanticIndex.reduceToDimensons(lsi, 1000)
    println(s"Reduced Dimensions: U (${lsi2.svD.U.rows} x ${lsi2.svD.U.cols}) S: ${lsi2.svD.S.length} Vt: (${lsi2.svD.Vt.rows} x ${lsi2.svD.Vt.cols})")


    // write the model to the zip file
    // the contents of the zip file contains csv data for use in visualisation tools such as R.
    val writeResult = LatentSemanticIndex.writeZipTemp(lsi2)(targetZip)

    writeResult match {
      case Success(flag) => flag match {
        case true => {
          println(s"Wrote model to $targetZip")
        }
        case _ => println(s"Failed to write model to $targetZip")
      }
      case _ => println(s"Failed to write $targetZip")
    }


    // write to binary format
    // this is useful for working in scala/java without the need for using external tools
    // it is also alot faster to read.
    val writeResult2 = LatentSemanticIndex.writeBinary(lsi2)(targetSer)
    writeResult2 match {
      case Some(flag) => flag match {
        case true => println(s"wrote model to $targetSer")
        case _ => println(s"failed to write model to $targetSer")
      }
      case _ => println(s"failed to write model to $targetSer")
    }
  }



  def main(args: Array[String]) = {

    buildModel()

  }
}

package au.id.cxd.math.example.text.model

import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.example.text.model.LsiReadModelExample.getClass
import au.id.cxd.math.example.text.model.config.ModelConfig
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.math.model.cluster._
import java.io.File

object LsiKMeansExample {
  val defaultConfig = "lsi-model.conf"

  def loadDocuments(config: ModelConfig) = {
    val url = getClass.getClassLoader().getResource(config.textFile)
    val inputCsv = url.getFile
    val data = new CsvReader().readCsv(new File(inputCsv))
    data
  }

  def readModel(config: ModelConfig) = {
    val readResult = LatentSemanticIndex.readBinary(config.targetSer)
    readResult match {
      case Some(lsi2) => {
        println(s"Dimensions: U (${lsi2.svD.U.rows} x ${lsi2.svD.U.cols}) S: ${lsi2.svD.S.length} Vt: (${lsi2.svD.Vt.rows} x ${lsi2.svD.Vt.cols})")
        Some(lsi2)
      }
      case _ => {
        println(s"Failed to read ${config.targetSer}")
        None
      }
    }
  }

  def main(args: Array[String]) = {
    val config = ModelConfig(args, defaultConfig)

    readModel(config).map {
      model =>
        // cluster documents using the object matrix

        val reduceLsi = LatentSemanticIndex.reduceToDimensons(model, 100)
        val docU = reduceLsi.svD.U
        val builder = KMeans(k=20)
        val result = builder.cluster(docU)
        ()
    }

    ()

  }
}

package au.id.cxd.math.example.text.model.config

import java.io.File
import java.net.URL

import au.id.cxd.math.example.text.model.LsiModelWriteExample.getClass
import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConversions._

class ModelConfig(val textFile:String,
                  val idCols:Array[Int],
                  val targetSer:String,
                  val targetZip:String,
                  val defaultQuery:Array[String]) {


  def getInputFile():URL = {
    print(s"Load $textFile")
    val temp = new File(textFile)
    if (temp.exists()) {
      temp.toURI.toURL
    } else getClass.getClassLoader().getResource(textFile)
  }

}
object ModelConfig {

  def loadConfig(path:String) = {
    val file = new File(path)
    file.exists() match {
      case true =>
        val conf = ConfigFactory.load()
        ConfigFactory.parseFile(file).withFallback(conf)
      case _ => ConfigFactory.load(path)
    }
  }

  def apply(args:Array[String], configPath:String) = {
    val config = args.length > 0 match {
      case true => loadConfig(args(0))
      case _ => loadConfig(configPath)
    }
    val textFile = config.getString("model.textData")
    val idCols = config.getIntList("model.csvIdColumns").map(_.toInt).toArray[Int]
    val targetSer = config.getString("model.targetSer")
    val targetZip = config.getString("model.targetZip")
    val defaultQuery:Array[String] = config.getStringList("model.defaultQuery").map(_.toString).toArray
    new ModelConfig (textFile,
      idCols,
      targetSer,
      targetZip,
      defaultQuery)
  }
}

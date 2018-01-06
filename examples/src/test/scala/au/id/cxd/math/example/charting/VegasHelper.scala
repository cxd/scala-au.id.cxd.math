package au.id.cxd.math.example.charting

import java.io.{File, PrintWriter}

import vegas.DSL.{ExtendedUnitSpecBuilder, SpecBuilder}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

object VegasHelper {
  import vegas.render.HTMLRenderer._

  private def runBrowser(html:String, fileName:String="temp.html") = {
    val fileout = new File(fileName)
    val writer = new PrintWriter(fileout)
    writer.write(html)
    writer.close()
    // could check other platforms and set an appropriate browser if I was using other platforms.
    val browser = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

    Runtime.getRuntime.exec(
      Array(browser, fileout.getAbsolutePath))

  }

  def showPlot(p:SpecBuilder, fileName:String="temp.html") = {

    val plotFrame = p.pageHTML()
    runBrowser(plotFrame, fileName)
  }

  def JSImports = List(
    "http://cdn.jsdelivr.net/webjars/org.webjars.bower/d3/3.5.17/d3.min.js",
    "https://cdnjs.cloudflare.com/ajax/libs/vega/3.0.8/vega.js",
    "https://cdnjs.cloudflare.com/ajax/libs/vega-lite/2.0.3/vega-lite.js",
    "https://cdnjs.cloudflare.com/ajax/libs/vega-embed/3.0.0-rc7/vega-embed.js"
  )

  def importsHTML(additionalImports: String*) = (JSImports ++ additionalImports).map { s =>
    "<script src=\"" + s + "\" charset=\"utf-8\"></script>"
  }.mkString("\n")

  def headerHTML(additionalImports: String*) =
    s"""
       |<html>
       |  <head>
       |    ${ importsHTML(additionalImports:_*) }
       |  </head>
       |  <body>
    """.stripMargin

  /**
    * show the plot and use a function to manipulate the json prior to saving.
    * @param p
    * @param fn
    * @param divName
    * @param fileName
    * @return
    */
  def transformAndShowPlot(p:SpecBuilder, fn:(String => String) = id => id, divName:String="", fileName:String="temp.html") = {
    val div = if (divName.equalsIgnoreCase("")) p.defaultName
    else divName
    val plotJson = p.specJson
    val updateSpec = fn(p.specJson).trim
    val json = parse(updateSpec)
    val newJson = json merge JObject("$schema" -> JString("https://vega.github.io/schema/vega-lite/v2.json"))
    val newSpec = compact(render(newJson))


    val pageData = s"""
       | <div id='$div'></div>
       | <script>
       |   var vegaSpec = $newSpec;
       |
       |   vegaEmbed("#$div", vegaSpec);
       | </script>
    """.stripMargin
    val footerHTML =
      """
        |  </body>
        |</html>
      """.stripMargin
    val html = headerHTML().trim + pageData + footerHTML.trim

    runBrowser(html, fileName)
  }


  /**
    * replace the mark using a value not yet implemented in vegas
    * @param spec
    * @param mark
    */
  def replaceMark(mark:String, spec:String):String = {
    implicit val formats = DefaultFormats
    val json = parse(spec)
    val newJson1 = json transformField {
      case JField("mark", JString(s)) => ("mark", JString(mark))
    }
    compact(render(newJson1))
  }
}

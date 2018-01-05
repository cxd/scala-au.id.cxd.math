package au.id.cxd.math.example.charting

import java.io.{File, PrintWriter}

import vegas.DSL.{ExtendedUnitSpecBuilder, SpecBuilder}

object VegasHelper {


  def showPlot(p:SpecBuilder, name:String="temp.html") = {

    import vegas.render.HTMLRenderer._

    val plotFrame = p.pageHTML()
    val fileout = new File(name)
    val writer = new PrintWriter(fileout)
    writer.write(plotFrame)
    writer.close()

    val browser = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

    Runtime.getRuntime.exec(
      Array(browser, fileout.getAbsolutePath))

  }
}

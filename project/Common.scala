import java.io.PrintWriter

import sbt.Keys._
import sbt._

import scala.io.Source

object Common extends AutoPlugin {


  // details here on how to post process the generated html and insert mathjax
  // http://stackoverflow.com/questions/15996651/is-there-a-way-to-include-math-formulae-in-scaladoc
  lazy val mathFormulaInDoc = TaskKey[Unit]("add MathJax script import in doc html to display nice latex formula")

  // function that find html files recursively
  def listHtmlFile(dir: java.io.File): List[java.io.File] = {
    dir.listFiles.toList.flatMap { f =>
      if (f.getName.endsWith(".html")) List(f)
      else if (f.isDirectory) listHtmlFile(f)
      else List[File]()
    }
  }

  val commonPluginSettings = Seq(

    mathFormulaInDoc := {
      val apiDir = (doc in (Compile)).value
      val docDirs = List(apiDir) // /"some"/"subfolder"  // in my case, only api/some/solder is parsed
      // will replace this "importTag" by "scriptLine
      val importTag = "##import MathJax"
      val scriptLine =
        """<script type="text/x-mathjax-config">
          |MathJax.Hub.Config({
          |tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]},
          |"HTML-CSS": {preferredFont: 'TeX'}
          |});
          |</script>
          |<script type="text/javascript" async src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML"></script>
          | """.stripMargin
      // find all html file and apply patch
      docDirs.foreach { docDir =>
        if (docDir.isDirectory)
          listHtmlFile(docDir).foreach { f =>
            val content = Source.fromFile(f).getLines().mkString("\n")
            if (content.contains(importTag)) {
              val writer = new PrintWriter(f)
              writer.write(content.replace(importTag, scriptLine))
              writer.close()
            }
          }
      }

    },

    // attach this task to doc task
    mathFormulaInDoc <<= mathFormulaInDoc triggeredBy (doc in Compile)

  )

}
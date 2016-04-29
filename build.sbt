import java.io.PrintWriter

import scala.io.Source
import com.typesafe.sbt.SbtGit.GitKeys._
import sbt.Keys._

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  version := "1.0",

  resolvers += Opts.resolver.sonatypeReleases,

  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  libraryDependencies ++= Seq(
    // other dependencies here
    "org.scalanlp" %% "breeze" % "0.11.2",
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    "org.scalanlp" %% "breeze-natives" % "0.11.2",
    // add breeze visualization
    "org.scalanlp" %% "breeze-viz" % "0.11.2",
    // add the scalaz library
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    // wrapper around jfreechart
    "com.github.wookietreiber" %% "scala-chart" % "0.5.0"
  )
)










// details here on how to post process the generated html and insert mathjax
// http://stackoverflow.com/questions/15996651/is-there-a-way-to-include-math-formulae-in-scaladoc
lazy val mathFormulaInDoc = taskKey[Unit]("add MathJax script import in doc html to display nice latex formula")

mathFormulaInDoc := {
  val apiDir = (doc in Compile).value
  val docDir = apiDir // /"some"/"subfolder"  // in my case, only api/some/solder is parsed
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

// attach this task to doc task
mathFormulaInDoc <<= mathFormulaInDoc triggeredBy (doc in Compile)

// function that find html files recursively
def listHtmlFile(dir: java.io.File): List[java.io.File] = {
  dir.listFiles.toList.flatMap { f =>
    if (f.getName.endsWith(".html")) List(f)
    else if (f.isDirectory) listHtmlFile(f)
    else List[File]()
  }
}

lazy val math = (project in file("math"))
  .settings(unidocSettings: _*)
  .settings(commonSettings: _*)
  .settings(

    name := "au.id.cxd.math"


  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    name := "au.id.cxd.math.examples"
  )
  .dependsOn(math)

lazy val root = (project in file("."))
  .aggregate(math, examples)


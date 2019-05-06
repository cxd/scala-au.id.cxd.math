import java.io.PrintWriter
import scala.io.Source
import com.typesafe.sbt.SbtGit.GitKeys._


lazy val breezeVersion = "0.13.2"


javaOptions += "-Xmx2G -Xms1G -XX:MaxPermSize=512M"


javaOptions ++= Seq(
  // -J params will be added as jvm paramete
  "-J-Xmx2g",
  "-J-Xms1g",
  "-J-XX:MaxPermSize=512m"
)



// details here on how to post process the generated html and insert mathjax
// http://stackoverflow.com/questions/15996651/is-there-a-way-to-include-math-formulae-in-scaladoc
lazy val mathFormulaInDoc = taskKey[File]("add MathJax script import in doc html to display nice latex formula")


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
    val apiDir = (doc in Compile).value
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
        |<script type="text/javascript" async src="https://cxd.github.io/scala-au.id.cxd.math/javascripts/MathJax-2.7.2/MathJax.js?config=TeX-AMS_CHTML"></script>
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
    apiDir
  }

)


lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",


  version := "1.0",

  resolvers ++= Seq(Resolver.sonatypeRepo("public"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")),

  coverageEnabled := false,

  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  libraryDependencies ++= Seq(
    //// scalax io
    //"com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
    // other dependencies here
    "org.scalanlp" %% "breeze" % breezeVersion,
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    "org.scalanlp" %% "breeze-natives" % breezeVersion,
    // add the scalaz library
    "org.scalaz" %% "scalaz-core" % "7.2.27",
    // json4s
    "org.json4s" %% "json4s-jackson" % "3.6.0-M2"
  )
)

lazy val uiDependencies = Seq(
  libraryDependencies ++= Seq(
    // add breeze visualization
    "org.scalanlp" %% "breeze-viz" % breezeVersion,
    // wrapper around jfreechart
    "com.github.wookietreiber" %% "scala-chart" % "0.5.1",
    // include kumo for tag cloud generation
    "com.kennycason" % "kumo" % "1.8",
    // vegas library
    "org.vegas-viz" %% "vegas" % "0.3.11",
    "org.vegas-viz" %% "vegas-macros" % "0.3.11"
  )
)

//test in assembly := {}

lazy val math = (project in file("math"))
  .settings(commonSettings: _*)
  .settings(commonPluginSettings:_*)
  .settings(

    name := "au.id.cxd.math",

    // note to cross compile for multiple versions of scala use the
    // > + compile
    // > + assembly
    // to manually switch between versions
    // > ++ 2.11.8
    // > ++ 2.12.2
    // this should generate multiple targets
    crossScalaVersions := Seq("2.11.8", "2.12.2"),

    libraryDependencies ++= Seq(
      // add breeze visualization
      "org.scalanlp" %% "breeze-viz" % breezeVersion % "test",
      // wrapper around jfreechart
      "com.github.wookietreiber" %% "scala-chart" % "0.5.1" % "test",
      // include kumo for tag cloud generation
      "com.kennycason" % "kumo" % "1.8" % "test"
    ),

    test in assembly := {},

    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }


  ).settings(Common.commonPluginSettings: _*)
  .enablePlugins(ScalaUnidocPlugin)

lazy val exclude = List(
  "jcommon-1.0.16.jar",
  "kumo-1.8.jar",
  "jfreechart-1.0.13.jar"
)

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(uiDependencies: _*)
  .settings(
    name := "au.id.cxd.math.examples",

    libraryDependencies ++= Seq(
      "jfree" % "jcommon" % "1.0.16" % "provided",
      "com.typesafe" % "config" % "1.3.2"
    ),

    aggregate in assembly := false,

    assemblyExcludedJars in assembly := {
      val cp = (fullClasspath in assembly).value
      cp filter { x => exclude.exists(item => x.data.getName.matches(item)) }
    }
  )
  .settings(Common.commonPluginSettings: _*)
  .dependsOn(math)


lazy val swing = (project in file("app"))
  .settings(commonSettings: _*)
  .settings(uiDependencies: _*)
  .settings(
    name := "au.id.cxd.math.app",

    aggregate in assembly := false,

    assemblyExcludedJars in assembly := {
      val cp = (fullClasspath in assembly).value
      cp filter { x => exclude.exists(item => x.data.getName.matches(item)) }
    }
  ).settings(Common.commonPluginSettings: _*)
  .dependsOn(math)


assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

lazy val root = (project in file("."))
  .aggregate(math, examples, swing)

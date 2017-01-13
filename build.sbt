import java.io.PrintWriter

import scala.io.Source
import com.typesafe.sbt.SbtGit.GitKeys._
import sbt.Keys._

val breezeVersion = "0.12"


javaOptions += "-Xmx2G -Xms1G -XX:MaxPermSize=512M"


javaOptions ++= Seq(
  // -J params will be added as jvm paramete
  "-J-Xmx2g",
  "-J-Xms1g",
  "-J-XX:MaxPermSize=512m"
)

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  version := "1.0",

  resolvers += Opts.resolver.sonatypeReleases,

  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  libraryDependencies ++= Seq(
    // other dependencies here
    "org.scalanlp" %% "breeze" % breezeVersion,
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    "org.scalanlp" %% "breeze-natives" % breezeVersion,
    // add breeze visualization
    "org.scalanlp" %% "breeze-viz" % breezeVersion,
    // add the scalaz library
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    // wrapper around jfreechart
    "com.github.wookietreiber" %% "scala-chart" % "0.5.0"
  )
)




lazy val math = (project in file("math"))
  .settings(unidocSettings: _*)
  .settings(commonSettings: _*)
  .settings(

    name := "au.id.cxd.math"

  ).settings(Common.commonPluginSettings: _*)

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    name := "au.id.cxd.math.examples"
  )
  .settings(Common.commonPluginSettings: _*)
  .dependsOn(math)


lazy val ui = (project in file("ui"))
  .settings(unidocSettings: _*)
  .settings(commonSettings: _*)
  .settings(
    name := "au.id.cxd.math.ui"
  )
  .settings(Common.commonPluginSettings: _*)
  .dependsOn(math)


lazy val root = (project in file("."))
  .aggregate(math, examples, ui)


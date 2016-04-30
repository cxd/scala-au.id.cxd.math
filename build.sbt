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


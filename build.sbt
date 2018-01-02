import java.io.PrintWriter

import scala.io.Source
import com.typesafe.sbt.SbtGit.GitKeys._
import sbt.Keys._
import AssemblyKeys._

val breezeVersion = "0.12"


javaOptions += "-Xmx2G -Xms1G -XX:MaxPermSize=512M"


javaOptions ++= Seq(
  // -J params will be added as jvm paramete
  "-J-Xmx2g",
  "-J-Xms1g",
  "-J-XX:MaxPermSize=512m"
)

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  // note to cross compile for multiple versions of scala use the
  // > + compile
  // > + assembly
  // to manually switch between versions
  // > ++ 2.11.8
  // > ++ 2.12.2
  // this should generate multiple targets
  // breeze does not seem to be available for 2.12
  crossScalaVersions := Seq("2.11.8"),

  version := "1.0",

  resolvers += Opts.resolver.sonatypeReleases,

  coverageEnabled := false,

  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  libraryDependencies ++= Seq(
    //// scalax io
    //"com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
    // other dependencies here
    "org.scalanlp" %% "breeze" % breezeVersion,
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    "org.scalanlp" %% "breeze-natives" % breezeVersion,
    // add the scalaz library
    "org.scalaz" %% "scalaz-core" % "7.1.0"
  )
)

lazy val uiDependencies = Seq(
  libraryDependencies ++= Seq(
    // add breeze visualization
    "org.scalanlp" %% "breeze-viz" % breezeVersion,
    // wrapper around jfreechart
    "com.github.wookietreiber" %% "scala-chart" % "0.5.0",
    // include kumo for tag cloud generation
    "com.kennycason" % "kumo" % "1.8",
    // vegas library
    "com.github.vegas-viz" %% "vegas" % "0.3.5",
    "com.github.vegas-viz" %% "vegas-macros" % "0.3.5"
  )
)

//test in assembly := {}

lazy val math = (project in file("math"))
  .settings(unidocSettings: _*)
  .settings(commonSettings: _*)
  .settings(assemblySettings: _*)
  .settings(

    name := "au.id.cxd.math",

    libraryDependencies ++= Seq(
      // add breeze visualization
      "org.scalanlp" %% "breeze-viz" % breezeVersion % "test",
      // wrapper around jfreechart
      "com.github.wookietreiber" %% "scala-chart" % "0.5.0" % "test",
      // include kumo for tag cloud generation
      "com.kennycason" % "kumo" % "1.8" % "test"
    ),

    test in assembly := {}


  ).settings(Common.commonPluginSettings: _*)

lazy val exclude = List(
  "jcommon-1.0.16.jar",
  "kumo-1.8.jar",
  "jfreechart-1.0.13.jar"
)

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(uiDependencies: _*)
  .settings(assemblySettings: _*)
  .settings(
    name := "au.id.cxd.math.examples",

    libraryDependencies ++= Seq(
      "jfree" % "jcommon" % "1.0.16" % "provided"
    ),

    excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
      cp filter {x => exclude.exists(item => x.data.getName.matches(item))}
    }
  )
  .settings(Common.commonPluginSettings: _*)
  .dependsOn(math)


lazy val swing = (project in file("app"))
  .settings(unidocSettings: _*)
  .settings(commonSettings: _*)
  .settings(uiDependencies: _*)
  .settings(
    name := "au.id.cxd.math.app"
  ).settings(Common.commonPluginSettings: _*)
  .dependsOn(math)




lazy val root = (project in file("."))
  .aggregate(math, examples, swing)

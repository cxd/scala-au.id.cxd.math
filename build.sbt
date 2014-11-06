
name := "au.id.cxd.math"

version := "1.0"

scalaVersion := "2.10.2"

resolvers += Opts.resolver.sonatypeReleases

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.8.1",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  "org.scalanlp" %% "breeze-natives" % "0.8.1",
  // add breeze visualization
  "org.scalanlp" %% "breeze-viz" % "0.5",
  // add the scalaz library
  "org.scalaz" %% "scalaz-core" % "7.1.0"
)
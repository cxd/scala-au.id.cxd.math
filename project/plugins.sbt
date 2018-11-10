logLevel := Level.Warn

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")

// sbt unidoc
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

// fat jar
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.8")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
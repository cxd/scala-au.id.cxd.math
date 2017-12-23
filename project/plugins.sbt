logLevel := Level.Warn

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.7.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.2")

// sbt unidoc
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.3")

// fat jar
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")


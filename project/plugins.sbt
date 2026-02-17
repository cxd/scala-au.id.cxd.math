logLevel := Level.Warn

resolvers +=  ("jgit-repo" at "http://download.eclipse.org/jgit/maven")
  .withAllowInsecureProtocol(true)

//addSbtPlugin("com.github.sbt" % "sbt-site" % "1.5.0")

//addSbtPlugin("com.github.sbt" % "sbt-ghpages" % "0.7.0")

// sbt unidoc
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")

// fat jar
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
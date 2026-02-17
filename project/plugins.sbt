logLevel := Level.Warn

resolvers +=  ("jgit-repo" at "http://download.eclipse.org/jgit/maven")
  .withAllowInsecureProtocol(true)

// sbt unidoc
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")

// fat jar
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
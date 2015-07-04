lazy val root = (project in file(".")).
  settings(
    organization := "hyperflux",
    name := "hyperflux-plugin",
    version := "0.1",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.11.7"
    )
  )

publishTo := Some(Resolver.file(
  "file",  new File("/share/dev/lib/maven/")
))

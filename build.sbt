lazy val root = (project in file(".")).
  settings(
    name := "hyperflux-plugin",
    version := "0.1",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.11.7"
    )
  )
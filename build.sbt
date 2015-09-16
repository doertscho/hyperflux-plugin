lazy val root = (project in file(".")).
  settings(
    organization := "hyperflux",
    name := "hyperflux-plugin",
    version := "0.1",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.11.7",
      "com.lihaoyi" %% "scalatags_sjs0.6" % "0.5.2",
      "hyperflux" %% "hyperflux-framework" % "0.1"
    )
  )

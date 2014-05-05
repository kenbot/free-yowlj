scalaVersion := "2.10.4"

val scalazVersion = "7.1.0-M5"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "org.scala-lang" % "scala-swing" % "2.10.4", 
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test") 

fork := true

fork in Test := true

initialCommands := """
  import yowfree._;
"""

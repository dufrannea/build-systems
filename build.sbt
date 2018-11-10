val dottyVersion = "0.9.0"
val scala212Version = "2.12.6"
val scala212Minor = "2.12"
val catsVersion = "1.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-cross",
    version := "0.1.0",

    libraryDependencies += ("org.typelevel" %% "cats-core" % catsVersion).withDottyCompat(scalaVersion.value),

    // To make the default compiler and REPL use Dotty
    scalaVersion := dottyVersion,

    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(dottyVersion, scala212Version)
  )

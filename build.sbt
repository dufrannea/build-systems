val dottyVersion = "0.18.1-RC1"
val scala212Version = "2.12.7"
//val catsVersion = "1.0.0"
val catsMtlVersion = "0.6.0"
val catsEffectVersion = "2.0.0-RC2"
lazy val root = project
  .in(file("."))
  .settings(
    name := "build-systems",
    version := "0.1.0",

    //libraryDependencies += ("org.typelevel" %% "cats-core" % catsVersion).withDottyCompat(scalaVersion.value),
    libraryDependencies += ("org.typelevel" %% "cats-effect" % catsEffectVersion).withDottyCompat(scalaVersion.value),
    libraryDependencies += ("org.typelevel" %% "cats-mtl-core" % catsMtlVersion).withDottyCompat(scalaVersion.value),


    // To make the default compiler and REPL use Dotty
    scalaVersion := dottyVersion,

    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(dottyVersion, scala212Version)
  )

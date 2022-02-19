val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "build-systems",
    version := "0.1.0",

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0",
    libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.2.1",

    // To make the default compiler and REPL use Dotty
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:higherKinds", // Allow higher-kinded types
      "-unchecked", // Enable additional warnings where generated code depends on assumptions.
      "-Xfatal-warnings", // Fail the compilation if there are any warnings.
      "-Xmax-inlines",
      "1000"
    ),
  )

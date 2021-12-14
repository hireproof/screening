val Version = new {
  val Cats = "2.7.0"
  val Munit = "0.7.29"
  val Scala213 = "2.13.7"
  val Scala3 = "3.1.0"
  val ScalaJavaTime = "2.3.0"
}

ThisBuild / crossScalaVersions := List(Version.Scala213, Version.Scala3)
ThisBuild / developers := List(Developer("taig", "Niklas Klein", "mail@taig.io", url("https://taig.io/")))
ThisBuild / dynverVTagPrefix := false
ThisBuild / homepage := Some(url("https://github.com/taig/inspector/"))
ThisBuild / licenses := List("MIT" -> url("https://raw.githubusercontent.com/taig/inspector/main/LICENSE"))
ThisBuild / scalaVersion := Version.Scala213
ThisBuild / versionScheme := Some("early-semver")

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(
    name := "inspector-core",
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
        Nil
  )

lazy val generic = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/generic"))
  .settings(
    name := "inspector-generic",
    libraryDependencies ++=
      Nil
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val jsonCirce = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/json-circe"))
  .settings(
    name := "inspector-json-circe",
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % "0.14.1" ::
        Nil,
    scalacOptions ++= {
      if (scalaVersion.value == Version.Scala213) "-Ypatmat-exhaust-depth" :: "40" :: Nil else Nil
    }
  )
  .dependsOn(generic % "compile->compile;test->test")

val Version = new {
  val Cats = "2.7.0"
  val Circe = "0.14.1"
  val Java = "11"
  val Munit = "0.7.29"
  val Scala213 = "2.13.7"
  val Scala3 = "3.1.0"
  val ScalaJavaTime = "2.3.0"
}

inThisBuild(
  Def.settings(
    crossScalaVersions := List(Version.Scala213, Version.Scala3),
    developers := List(Developer("taig", "Niklas Klein", "niklas@hireproof.io", url("https://taig.io/"))),
    dynverVTagPrefix := false,
    homepage := Some(url("https://github.com/hireproof/screening/")),
    licenses := List("MIT" -> url("https://raw.githubusercontent.com/hireproof/screening/main/LICENSE")),
    organization := "io.hireproof",
    scalaVersion := Version.Scala213,
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    versionScheme := Some("early-semver")
  )
)

enablePlugins(BlowoutYamlPlugin)

noPublishSettings

blowoutGenerators ++= {
  val workflows = file(".github") / "workflows"

  BlowoutYamlGenerator.lzy(workflows / "main.yml", GithubActionsGenerator.main(Version.Java)) ::
    BlowoutYamlGenerator.lzy(workflows / "pull-request.yml", GithubActionsGenerator.pullRequest(Version.Java)) ::
    Nil
}

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(
    name := "screening-core",
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
    name := "screening-generic"
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val jsonCirce = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/json-circe"))
  .settings(
    name := "screening-json-circe",
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        Nil,
    scalacOptions ++= {
      if (scalaVersion.value == Version.Scala213) "-Ypatmat-exhaust-depth" :: "off" :: Nil else Nil
    }
  )
  .dependsOn(generic % "compile->compile;test->test")

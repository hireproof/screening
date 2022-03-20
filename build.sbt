import sbtcrossproject.CrossProject

val Version = new {
  val Cats = "2.7.0"
  val Circe = "0.14.1"
  val DisciplineMunit = "1.0.9"
  val Java = "11"
  val Munit = "0.7.29"
  val Scala213 = "2.13.8"
  val Scala3 = "3.1.1"
  val ScalaJavaTime = "2.3.0"
}

def module(identifier: Option[String]): CrossProject = {
  val platforms = JVMPlatform :: identifier.map(_ => JSPlatform).toList
  CrossProject(identifier.getOrElse("root"), file(identifier.fold(".")("modules/" + _)))(platforms: _*)
    .crossType(CrossType.Pure)
    .build()
    .settings(name := "screening" + identifier.fold("")("-" + _))
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

lazy val root = module(identifier = None)
  .enablePlugins(BlowoutYamlPlugin)
  .settings(noPublishSettings)
  .settings(
    blowoutGenerators ++= {
      val github = file(".github")
      val workflows = github / "workflows"

      BlowoutYamlGenerator.lzy(workflows / "main.yml", GithubActionsGenerator.main(Version.Java)) ::
        BlowoutYamlGenerator.lzy(workflows / "pull-request.yml", GithubActionsGenerator.pullRequest(Version.Java)) ::
        Nil
    }
  )
  .aggregate(core, jsonCirce)

lazy val core = module(identifier = Some("core"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        "org.typelevel" %%% "cats-laws" % Version.Cats % "test" ::
        "org.typelevel" %%% "discipline-munit" % Version.DisciplineMunit % "test" ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
        Nil
  )

lazy val jsonCirce = module(identifier = Some("json-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        Nil,
    scalacOptions ++= {
      if (scalaVersion.value == Version.Scala213) "-Ypatmat-exhaust-depth" :: "off" :: Nil else Nil
    }
  )
  .dependsOn(core % "compile->compile;test->test")

import io.circe.Json
import io.circe.syntax._

object GithubActionsGenerator {
  object Step {
    def setupJava(version: String): Json = Json.obj(
      "name" := "Setup Java JDK",
      "uses" := "actions/setup-java@v2.3.1",
      "with" := Json.obj(
        "distribution" := "temurin",
        "java-version" := version
      )
    )

    val Checkout: Json = Json.obj(
      "name" := "Checkout",
      "uses" := "actions/checkout@v2.4.0",
      "with" := Json.obj(
        "fetch-depth" := 0
      )
    )

    val Cache: Json = Json.obj(
      "name" := "Cache",
      "uses" := "coursier/cache-action@v6.3"
    )
  }

  object Job {
    def lint(javaVersion: String): Json = Json.obj(
      "name" := "Fatal warnings and code formatting",
      "runs-on" := "ubuntu-latest",
      "steps" := List(
        Step.setupJava(javaVersion),
        Step.Checkout,
        Step.Cache,
        Json.obj(
          "name" := "Workflows",
          "run" := "sbt blowoutCheck"
        ),
        Json.obj(
          "name" := "Code formatting",
          "run" := "sbt scalafmtCheckAll"
        ),
        Json.obj(
          "name" := "Fatal warnings",
          "run" := "sbt -Dmode=strict +compile"
        )
      )
    )

    def test(javaVersion: String): Json = Json.obj(
      "name" := "Unit tests",
      "runs-on" := "ubuntu-latest",
      "steps" := List(
        Step.setupJava(javaVersion),
        Step.Checkout,
        Step.Cache,
        Json.obj(
          "name" := "Tests",
          "run" := "sbt +test"
        )
      )
    )
  }

  def main(javaVersion: String): Json = Json.obj(
    "name" := "CI & CD",
    "on" := Json.obj(
      "push" := Json.obj(
        "branches" := List("main"),
        "tags" := List("*.*.*")
      )
    ),
    "jobs" := Json.obj(
      "lint" := Job.lint(javaVersion),
      "test" := Job.test(javaVersion),
      "deploy" := Json.obj(
        "name" := "🚀 Deploy",
        "runs-on" := "ubuntu-latest",
        "needs" := List("test", "lint"),
        "steps" := List(
          Step.setupJava(javaVersion),
          Step.Checkout,
          Step.Cache,
          Json.obj(
            "name" := "Release",
            "run" := "sbt ci-release",
            "env" := Json.obj(
              "PGP_PASSPHRASE" := "${{secrets.PGP_PASSPHRASE}}",
              "PGP_SECRET" := "${{secrets.PGP_SECRET}}",
              "SONATYPE_PASSWORD" := "${{secrets.SONATYPE_PASSWORD}}",
              "SONATYPE_USERNAME" := "${{secrets.SONATYPE_USERNAME}}"
            )
          )
        )
      )
    )
  )

  def pullRequest(javaVersion: String): Json = Json.obj(
    "name" := "CI",
    "on" := Json.obj(
      "pull_request" := Json.obj(
        "branches" := List("main")
      )
    ),
    "jobs" := Json.obj(
      "lint" := Job.lint(javaVersion),
      "test" := Job.test(javaVersion)
    )
  )
}

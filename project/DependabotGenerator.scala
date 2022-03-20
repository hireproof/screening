import io.circe.Json
import io.circe.syntax._

object DependabotGenerator {
  def apply(): Json = Json.obj(
    "version" := 2,
    "updates" := Json.obj(
      "package-ecosystem" := "github-actions",
      "directory" := "/",
      "schedule" := Json.obj(
        "interval" := "daily"
      )
    )
  )
}

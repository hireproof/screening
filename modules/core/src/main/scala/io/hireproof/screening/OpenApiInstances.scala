package io.hireproof.screening

import io.hireproof.openapi.syntax._
import io.hireproof.openapi.{Encoder, OpenApi}

import scala.concurrent.duration.{FiniteDuration, TimeUnit}

trait OpenApiInstances {
  implicit val encoderTimeUnit: Encoder[TimeUnit] = Encoder[String].contramap(_.name())

  implicit val encoderFiniteDuration: Encoder[FiniteDuration] = Encoder.instance { duration =>
    OpenApi.obj("length" := duration.length, "unit" := duration.unit)
  }
}

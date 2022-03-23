package io.hireproof.screening

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{CursorOp, Decoder, Encoder, Json, JsonObject, KeyDecoder, KeyEncoder}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

trait CirceInstances {
  implicit val decoderTimeUnit: Decoder[TimeUnit] = Decoder[String].emap { value =>
    Either.catchOnly[IllegalArgumentException](TimeUnit.valueOf(value)).leftMap(_ => "TimeUnit")
  }

  implicit val encoderTimeUnit: Encoder[TimeUnit] = Encoder[String].contramap(_.name())

  implicit val decoderFiniteDuration: Decoder[FiniteDuration] = Decoder.instance { cursor =>
    for {
      length <- cursor.get[Long]("length")
      unit <- cursor.get[TimeUnit]("unit")
    } yield FiniteDuration(length, unit)
  }

  implicit val encoderFiniteDuration: Encoder[FiniteDuration] = Encoder.instance { duration =>
    Json.obj("length" := duration.length, "unit" := duration.unit)
  }

  implicit val decoderConstraintRule: Decoder[Constraint.Rule] = Decoder.instance { cursor =>
    for {
      identifier <- cursor.get[String]("identifier").map(Constraint.Identifier.apply)
      reference <- cursor.get[Option[Json]]("reference")
      delta <- cursor.get[Option[Double]]("delta")
      equal <- cursor.get[Option[Boolean]]("equal")
    } yield Constraint.Rule(identifier, reference, delta, equal)
  }

  implicit val decoderConstraint: Decoder[Constraint] = decoderConstraintRule.or {
    Decoder.instance { cursor =>
      for {
        left <- cursor.get[Set[Constraint]]("left")
        right <- cursor.get[Set[Constraint]]("right")
      } yield Constraint.Or(left, right)
    }
  }

  implicit val encoderConstraint: Encoder.AsObject[Constraint] = Encoder.AsObject.instance {
    case Constraint.Or(left, right) =>
      JsonObject("left" := left, "right" := right)
    case Constraint.Rule(identifier, reference, delta, equal) =>
      JsonObject(
        "identifier" := identifier.value,
        "reference" := reference,
        "delta" := delta,
        "equal" := equal
      ).filter { case (_, json) => !json.isNull }
  }

  implicit val decoderViolation: Decoder[Violation] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "validation" =>
        (cursor.as[Constraint], cursor.get[Json]("actual")).mapN(Violation.Validation.apply)
      case "conflict" => cursor.get[Json]("actual").map(Violation.Conflict.apply)
      case "invalid" =>
        (cursor.get[Option[Json]]("reference"), cursor.get[Json]("actual")).mapN(Violation.Invalid.apply)
      case "missing" => cursor.get[Option[Json]]("reference").map(Violation.Missing.apply)
      case "unknown" => cursor.get[Json]("actual").map(Violation.Unknown.apply)
    }
  }

  implicit val encoderViolation: Encoder.AsObject[Violation] = Encoder.AsObject.instance {
    case Violation.Validation(constraint, actual) =>
      JsonObject("type" := "validation", "actual" := actual) deepMerge constraint.asJsonObject
    case Violation.Conflict(actual) => JsonObject("type" := "conflict", "actual" := actual)
    case Violation.Invalid(reference, actual) =>
      JsonObject("type" := "invalid", "reference" := reference, "actual" := actual)
    case Violation.Missing(reference) => JsonObject("type" := "missing", "reference" := reference)
    case Violation.Unknown(actual)    => JsonObject("type" := "unknown", "actual" := actual)
  }

  implicit val keyEncoderCursorHistory: KeyEncoder[List[CursorOp]] = KeyEncoder.instance(CursorOp.opsToPath)

  implicit val keyEncoderSelectionHistory: KeyEncoder[Selection.History] = KeyEncoder.instance(_.toJsonPath)

  implicit val keyDecoderSelectionHistory: KeyDecoder[Selection.History] =
    KeyDecoder.instance(Selection.History.parse(_).toOption)

  implicit val decoderViolations: Decoder[Violations] =
    Decoder[NonEmptyMap[Selection.History, NonEmptyList[Violation]]].map(Violations.apply)

  implicit val encoderViolations: Encoder[Violations] =
    Encoder[NonEmptyMap[Selection.History, NonEmptyList[Violation]]].contramap(_.toNem)
}

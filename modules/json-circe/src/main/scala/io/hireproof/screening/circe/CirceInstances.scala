package io.hireproof.screening.circe

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Error => _, _}
import io.hireproof.screening.{Actual, Constraint, Reference, Selection, Violation, Violations}

trait CirceInstances {
  implicit val decoderConstraintValue: Decoder[Constraint.Rule] = Decoder.instance { cursor =>
    for {
      identifier <- cursor.get[String]("identifier").map(Constraint.Identifier.apply)
      reference <- cursor.get[Option[String]]("identifier").map(_.map(Reference.apply))
      delta <- cursor.get[Option[String]]("delta").map(_.map(Constraint.Delta.apply))
      equal <- cursor.get[Option[Boolean]]("equal")
    } yield Constraint.Rule(identifier, reference, delta, equal)
  }

  implicit val decoderConstraint: Decoder[Constraint] = decoderConstraintValue.or {
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
        "reference" := reference.map(_.value),
        "delta" := delta.map(_.value),
        "equal" := equal
      ).filter { case (_, json) => !json.isNull }
  }

  implicit val decoderViolation: Decoder[Violation] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "validation" =>
        (cursor.as[Constraint], cursor.get[String]("actual").map(Actual.apply)).mapN(Violation.Validation.apply)
      case "conflict" => cursor.get[String]("actual").map(Actual.apply).map(Violation.Conflict.apply)
      case "invalid" =>
        (
          cursor.get[Option[String]]("reference").map(_.map(Reference.apply)),
          cursor.get[String]("actual").map(Actual.apply)
        ).mapN(Violation.Invalid.apply)
      case "missing" => cursor.get[Option[String]]("reference").map(_.map(Reference.apply)).map(Violation.Missing.apply)
      case "unknown" => cursor.get[String]("actual").map(Actual.apply).map(Violation.Unknown.apply)
    }
  }

  implicit val encoderViolation: Encoder.AsObject[Violation] = Encoder.AsObject.instance {
    case Violation.Validation(constraint, actual) =>
      JsonObject("type" := "validation", "actual" := actual.value) deepMerge constraint.asJsonObject
    case Violation.Conflict(actual) => JsonObject("type" := "conflict", "actual" := actual.value)
    case Violation.Invalid(reference, actual) =>
      JsonObject("type" := "invalid", "reference" := reference.map(_.value), "actual" := actual.value)
    case Violation.Missing(reference) => JsonObject("type" := "missing", "reference" := reference.map(_.value))
    case Violation.Unknown(actual)    => JsonObject("type" := "unknown", "actual" := actual.value)
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

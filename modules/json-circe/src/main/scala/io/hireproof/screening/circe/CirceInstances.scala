package io.hireproof.screening.circe

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Error => _, _}
import io.hireproof.screening.{Actual, Constraint, Error, Reference, Selection, Violations}

trait CirceInstances {
  implicit val decoderConstraint: Decoder[Constraint] = Decoder.instance { cursor =>
    for {
      identifier <- cursor.get[String]("identifier").map(Constraint.Identifier.apply)
      reference <- cursor.get[Option[String]]("identifier").map(_.map(Reference.apply))
      delta <- cursor.get[Option[String]]("delta").map(_.map(Constraint.Delta.apply))
      equal <- cursor.get[Option[Boolean]]("equal")
    } yield Constraint(identifier, reference, delta, equal)
  }

  implicit val encoderConstraint: Encoder.AsObject[Constraint] = Encoder.AsObject.instance { constraint =>
    JsonObject(
      "identifier" := constraint.identifier.value,
      "reference" := constraint.reference.map(_.value),
      "delta" := constraint.delta.map(_.value),
      "equal" := constraint.equal
    ).filter { case (_, json) => !json.isNull }
  }

  implicit val decoderError: Decoder[Error] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "constraint" =>
        (cursor.as[Constraint], cursor.get[String]("actual").map(Actual.apply)).mapN(Error.BrokenConstraint)
      case "conflict" => cursor.get[String]("actual").map(Actual.apply).map(Error.Conflict)
      case "invalid" =>
        (
          cursor.get[Option[String]]("reference").map(_.map(Reference.apply)),
          cursor.get[String]("actual").map(Actual.apply)
        ).mapN(Error.Invalid)
      case "missing" => cursor.get[Option[String]]("reference").map(_.map(Reference.apply)).map(Error.Missing)
      case "unknown" => cursor.get[String]("actual").map(Actual.apply).map(Error.Unknown)
    }
  }

  implicit val encoderError: Encoder.AsObject[Error] = Encoder.AsObject.instance {
    case Error.BrokenConstraint(constraint, actual) =>
      JsonObject("type" := "constraint", "actual" := actual.value) deepMerge constraint.asJsonObject
    case Error.Conflict(actual) => JsonObject("type" := "conflict", "actual" := actual.value)
    case Error.Invalid(reference, actual) =>
      JsonObject("type" := "invalid", "reference" := reference.map(_.value), "actual" := actual.value)
    case Error.Missing(reference) => JsonObject("type" := "missing", "reference" := reference.map(_.value))
    case Error.Unknown(actual)    => JsonObject("type" := "unknown", "actual" := actual.value)
  }

  implicit val keyEncoderCursorHistory: KeyEncoder[List[CursorOp]] = KeyEncoder.instance(CursorOp.opsToPath)

  implicit val keyEncoderSelectionHistory: KeyEncoder[Selection.History] = KeyEncoder.instance(_.toJsonPath)

  implicit val keyDecoderSelectionHistory: KeyDecoder[Selection.History] =
    KeyDecoder.instance(Selection.History.parse(_).toOption)

  implicit def decoderViolations[A: Decoder]: Decoder[Violations[A]] =
    Decoder[NonEmptyMap[Selection.History, NonEmptyList[A]]].map(Violations.apply)

  implicit def encoderViolations[A: Encoder]: Encoder[Violations[A]] =
    Encoder[NonEmptyMap[Selection.History, NonEmptyList[A]]].contramap(_.toNem)
}

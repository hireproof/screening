package io.hireproof.screening.circe

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.hireproof.screening.{Selection, Validation}

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

trait CirceInstances {
  private object Keys {
    val Actual = "actual"
    val Variant = "variant"
    val Error = "error"
    val Reference = "reference"
    val Regex = "regex"
    val Type = "type"
  }

  private object Types {
    val Collection = "collection"
    val Conflict = "conflict"
    val Date = "date"
    val Duration = "duration"
    val Invalid = "invalid"
    val Mapping = "mapping"
    val Missing = "missing"
    val Not = "not"
    val Number = "number"
    val Parsing = "parsing"
    val Required = "requires"
    val Text = "text"
    val Unknown = "unknown"
  }

  private object Variants {
    val After = "after"
    val AfterEqual = "afterEqual"
    val AtLeast = "atLeast"
    val AtLeastEqual = "atLeastEqual"
    val AtMost = "atMost"
    val AtMostEqual = "atMostEqual"
    val Before = "before"
    val BeforeEqual = "beforeEqual"
    val Contains = "contains"
    val Email = "email"
    val Equal = "equal"
    val Exactly = "exactly"
    val GreaterThan = "greaterThan"
    val GreaterThanEqual = "greaterThanEqual"
    val LessThan = "lessThan"
    val LessThanEqual = "lessThanEqual"
    val Matches = "matches"
  }

  private object decoder {
    def apply[A: Decoder](cursor: HCursor): Either[DecodingFailure, A] = cursor.get[A](Keys.Actual)

    def reference[A: Decoder, B: Decoder](cursor: HCursor): Either[DecodingFailure, (A, B)] =
      (cursor.get[A](Keys.Reference), decoder[B](cursor)).tupled
  }

  private object encoder {
    def apply[A: Encoder](tpe: String, actual: A): JsonObject =
      JsonObject(Keys.Type := tpe, Keys.Actual := actual)

    def variant[A: Encoder](context: String, variant: String, actual: A): JsonObject =
      encoder(context, actual) deepMerge JsonObject(Keys.Variant := variant)

    def reference[A: Encoder, B: Encoder](context: String, variant: String, reference: A, actual: B): JsonObject =
      encoder.variant(context, variant, actual) deepMerge JsonObject(Keys.Reference := reference)
  }

  implicit private val decoderDuration: Decoder[FiniteDuration] = Decoder.instance { cursor =>
    val length = cursor.get[Long]("length")
    val unit = cursor.get[String]("unit").flatMap { value =>
      Either
        .catchOnly[IllegalArgumentException](TimeUnit.valueOf(value))
        .leftMap(_ => DecodingFailure("FiniteDuration", cursor.history))
    }

    (length, unit).mapN(FiniteDuration.apply)
  }

  implicit private val encoderDuration: Encoder[FiniteDuration] = Encoder.instance { duration =>
    Json.obj(
      "length" := duration.length,
      "unit" := duration.unit.name()
    )
  }

  implicit val decoderInspectorValidationViolation: Decoder[Validation.Violation] = ???
//  Decoder.instance { cursor =>
//    (cursor.get[String](Keys.Type), cursor.get[Option[String]](Keys.Variant)).tupled.flatMap {
//      // format: off
//      case (Types.Collection, Some(Variants.AtLeast)) => decoder.reference[Long, Long](cursor).map { case (reference, actual) => Validation.Violation.Collection.AtLeast(equal = false, reference, actual) }
//      case (Types.Collection, Some(Variants.AtLeastEqual)) => decoder.reference[Long, Long](cursor).map { case (reference, actual) => Validation.Violation.Collection.AtLeast(equal = true, reference, actual) }
//      case (Types.Collection, Some(Variants.AtMost)) => decoder.reference[Long, Long](cursor).map { case (reference, actual) => Validation.Violation.Collection.AtMost(equal = false, reference, actual) }
//      case (Types.Collection, Some(Variants.AtMostEqual)) => decoder.reference[Long, Long](cursor).map { case (reference, actual) => Validation.Violation.Collection.AtMost(equal = true, reference, actual) }
//      case (Types.Collection, Some(Variants.Contains)) => decoder.reference[String, List[String]](cursor).map { case (reference, actual) => Validation.Violation.Collection.Contains(reference, actual) }
//      case (Types.Collection, Some(Variants.Exactly)) => decoder.reference[Long, Long](cursor).map { case (reference, actual) => Validation.Violation.Collection.Exactly(reference,actual) }
//      case (Types.Conflict, None) => decoder[String](cursor).map(Validation.Violation.Conflict.apply)
//      case (Types.Date, Some(Variants.After)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Violation.Date.After(equal = false, reference, actual) }
//      case (Types.Date, Some(Variants.AfterEqual)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Violation.Date.After(equal = true, reference, actual) }
//      case (Types.Date, Some(Variants.Before)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Violation.Date.Before(equal = false, reference, actual) }
//      case (Types.Date, Some(Variants.BeforeEqual)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Violation.Date.Before(equal = true, reference, actual) }
//      case (Types.Duration, Some(Variants.AtMost)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Violation.Duration.AtMost(equal = false, reference, actual) }
//      case (Types.Duration, Some(Variants.AtMostEqual)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Violation.Duration.AtMost(equal = true, reference, actual) }
//      case (Types.Duration, Some(Variants.AtLeast)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Violation.Duration.AtLeast(equal = false, reference, actual) }
//      case (Types.Duration, Some(Variants.AtLeastEqual)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Violation.Duration.AtLeast(equal = true, reference, actual) }
//      case (Types.Duration, Some(Variants.Exactly)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Violation.Duration.Exactly(reference, actual) }
//      case (Types.Invalid, None) => decoder.reference[Option[String], String](cursor).map { case (reference, actual) => Validation.Violation.Invalid(reference, actual) }
//      case (Types.Mapping, None) => (cursor.get[Option[Set[String]]](Keys.Reference), cursor.get[String](Keys.Actual)).mapN(Validation.Violation.Mapping.apply)
//      case (Types.Missing, None) => cursor.get[Option[String]](Keys.Reference).map(Validation.Violation.Missing.apply)
//      case (Types.Not, None) => cursor.get[Validation.Violation](Keys.Error)
//      case (Types.Number, Some(Variants.Equal)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Violation.Number.Equal(reference, actual) }
//      case (Types.Number, Some(Variants.GreaterThan)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Violation.Number.GreaterThan(equal = false, reference, actual) }
//      case (Types.Number, Some(Variants.GreaterThanEqual)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Violation.Number.GreaterThan(equal = true, reference, actual) }
//      case (Types.Number, Some(Variants.LessThan)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Violation.Number.LessThan(equal = false, reference, actual) }
//      case (Types.Number, Some(Variants.LessThanEqual)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Violation.Number.LessThan(equal = true, reference, actual) }
//      case (Types.Parsing, None) => decoder.reference[String, String](cursor).map { case (reference, actual) => Validation.Violation.Parsing(reference, actual) }
//      case (Types.Required, None) => Validation.Violation.Optional.Required.asRight
//      case (Types.Text, Some(Variants.AtLeast)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Violation.Text.AtLeast(equal = false, reference, actual) }
//      case (Types.Text, Some(Variants.AtLeastEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Violation.Text.AtLeast(equal = true, reference, actual) }
//      case (Types.Text, Some(Variants.AtMost)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Violation.Text.AtMost(equal = false, reference, actual) }
//      case (Types.Text, Some(Variants.AtMostEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Violation.Text.AtMost(equal = true, reference, actual) }
//      case (Types.Text, Some(Variants.Email)) => decoder[String](cursor).map(Validation.Violation.Text.Email.apply)
//      case (Types.Text, Some(Variants.Equal)) => decoder.reference[String, String](cursor).map { case (reference, actual) => Validation.Violation.Text.Equal(reference, actual) }
//      case (Types.Text, Some(Variants.Exactly)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Violation.Text.Exactly(reference, actual) }
//      case (Types.Text, Some(Variants.Matches)) => (cursor.get[String](Keys.Regex).map(new Regex(_)), decoder[String](cursor)).mapN(Validation.Violation.Text.Matches.apply)
//      case (Types.Unknown, None) => decoder[String](cursor).map(Validation.Violation.Unknown.apply)
//      case (tpe, variant) => DecodingFailure(s"Invalid validation error: type = $tpe, variant = $variant", cursor.history).asLeft
//      // format: on
//    }
//  }

  implicit val encoderInspectorValidationViolation: Encoder.AsObject[Validation.Violation] = ???
//  Encoder.AsObject.instance {
//    // format: off
//    case Validation.Violation.Collection.AtLeast(false, reference, actual) => encoder.reference(Types.Collection, Variants.AtLeast, reference, actual)
//    case Validation.Violation.Collection.AtLeast(true, reference, actual) => encoder.reference(Types.Collection, Variants.AtLeastEqual, reference, actual)
//    case Validation.Violation.Collection.AtMost(false, reference, actual) => encoder.reference(Types.Collection, Variants.AtMost, reference, actual)
//    case Validation.Violation.Collection.AtMost(true, reference, actual) => encoder.reference(Types.Collection, Variants.AtMostEqual, reference, actual)
//    case Validation.Violation.Collection.Contains(reference, actual) => encoder.reference(Types.Collection, Variants.Contains, reference, actual)
//    case Validation.Violation.Collection.Exactly(reference, actual) => encoder.reference(Types.Collection, Variants.Exactly, reference, actual)
//    case Validation.Violation.Conflict(actual) => encoder(Types.Conflict, actual)
//    case Validation.Violation.Date.After(false, reference, actual) => encoder.reference(Types.Date, Variants.After, reference, actual)
//    case Validation.Violation.Date.After(true, reference, actual) => encoder.reference(Types.Date, Variants.AfterEqual, reference, actual)
//    case Validation.Violation.Date.Before(false, reference, actual) => encoder.reference(Types.Date, Variants.Before, reference, actual)
//    case Validation.Violation.Date.Before(true, reference, actual) => encoder.reference(Types.Date, Variants.BeforeEqual, reference, actual)
//    case Validation.Violation.Duration.AtLeast(false, reference, actual) => encoder.reference(Types.Duration, Variants.AtLeast, reference, actual)
//    case Validation.Violation.Duration.AtLeast(true, reference, actual) => encoder.reference(Types.Duration, Variants.AtLeastEqual, reference, actual)
//    case Validation.Violation.Duration.AtMost(false, reference, actual) => encoder.reference(Types.Duration, Variants.AtMost, reference, actual)
//    case Validation.Violation.Duration.AtMost(true, reference, actual) => encoder.reference(Types.Duration, Variants.AtMostEqual, reference, actual)
//    case Validation.Violation.Duration.Exactly(reference, actual) => encoder.reference(Types.Duration, Variants.Exactly, reference, actual)
//    case Validation.Violation.Invalid(reference, actual) => encoder(Types.Invalid, actual).add(Keys.Reference, reference.asJson)
//    case Validation.Violation.Mapping(references, actual) => JsonObject(Keys.Type := Types.Mapping, Keys.Reference := references, Keys.Actual := actual)
//    case Validation.Violation.Missing(reference) => JsonObject(Keys.Type := Types.Missing, Keys.Reference := reference)
//    case Validation.Violation.Not(error) => JsonObject(Keys.Type := Types.Not, Keys.Error := error.asJsonObject)
//    case Validation.Violation.Number.Equal(reference, actual) => encoder.reference(Types.Number, Variants.Equal, reference, actual)
//    case Validation.Violation.Number.GreaterThan(false, reference, actual) => encoder.reference(Types.Number, Variants.GreaterThan, reference, actual)
//    case Validation.Violation.Number.GreaterThan(true, reference, actual) => encoder.reference(Types.Number, Variants.GreaterThanEqual, reference, actual)
//    case Validation.Violation.Number.LessThan(false, reference, actual) => encoder.reference(Types.Number, Variants.LessThan, reference, actual)
//    case Validation.Violation.Number.LessThan(true, reference, actual) => encoder.reference(Types.Number, Variants.LessThanEqual, reference, actual)
//    case Validation.Violation.Optional.Required => JsonObject(Keys.Type := Types.Required)
//    case Validation.Violation.Parsing(reference, actual) => encoder(Types.Parsing, actual) deepMerge JsonObject(Keys.Reference := reference)
//    case Validation.Violation.Text.AtLeast(false, reference, actual) => encoder.reference(Types.Text, Variants.AtLeast, reference, actual)
//    case Validation.Violation.Text.AtLeast(true, reference, actual) => encoder.reference(Types.Text, Variants.AtLeastEqual, reference, actual)
//    case Validation.Violation.Text.AtMost(false, reference, actual) => encoder.reference(Types.Text, Variants.AtMost, reference, actual)
//    case Validation.Violation.Text.AtMost(true, reference, actual) => encoder.reference(Types.Text, Variants.AtMostEqual, reference, actual)
//    case Validation.Violation.Text.Email(actual) => encoder.variant(Types.Text, Variants.Email, actual)
//    case Validation.Violation.Text.Equal(reference, actual) => encoder.reference(Types.Text, Variants.Equal, reference, actual)
//    case Validation.Violation.Text.Exactly(reference, actual) => encoder.reference(Types.Text, Variants.Exactly, reference, actual)
//    case Validation.Violation.Text.Matches(regex, actual) => encoder.variant(Types.Text, Variants.Matches, actual) deepMerge JsonObject(Keys.Regex := regex.toString())
//    case Validation.Violation.Unknown(actual) => encoder(Types.Unknown, actual)
//    // format: on
//  }

  implicit val keyEncoderCursorHistory: KeyEncoder[List[CursorOp]] = KeyEncoder.instance(CursorOp.opsToPath)

  implicit val encoderX: Encoder[Either[String, NonEmptyList[Validation.Violation]]] =
    Encoder.instance {
      case Right(errors) => Json.obj(Keys.Type := "errors", "errors" := errors)
      case Left(failure) => Json.obj(Keys.Type := "failure", "message" := failure)
    }

  implicit val keyEncoderSelectionHistory: KeyEncoder[Selection.History] = KeyEncoder.instance(_.toJsonPath)

  implicit val keyDecoderSelectionHistory: KeyDecoder[Selection.History] =
    KeyDecoder.instance(Selection.History.parse(_).toOption)

  implicit val decoderValidationViolations: Decoder[Validation.Violations] =
    Decoder[NonEmptyMap[Selection.History, NonEmptyList[Validation.Violation]]].map(Validation.Violations.apply)

  implicit val encoderValidationViolations: Encoder[Validation.Violations] =
    Encoder[NonEmptyMap[Selection.History, NonEmptyList[Validation.Violation]]].contramap(_.toNem)
}

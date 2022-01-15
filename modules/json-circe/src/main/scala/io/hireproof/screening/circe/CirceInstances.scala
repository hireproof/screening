package io.hireproof.screening.circe

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.hireproof.screening.Validation.Parsing.Value
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
    val Mapping = "mapping"
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

  private val parsingValueToString: Value => String = {
    case Validation.Parsing.Value.BigDecimal => "bigDecimal"
    case Validation.Parsing.Value.BigInt     => "bigInt"
    case Validation.Parsing.Value.Double     => "double"
    case Validation.Parsing.Value.Float      => "float"
    case Validation.Parsing.Value.Instant    => "instant"
    case Validation.Parsing.Value.Int        => "int"
    case Validation.Parsing.Value.Long       => "long"
    case Validation.Parsing.Value.Short      => "short"
  }

  private val parsingValueFromString: String => Option[Validation.Parsing.Value] = PartialFunction.condOpt(_) {
    case "bigDecimal" => Validation.Parsing.Value.BigDecimal
    case "bigInt"     => Validation.Parsing.Value.BigInt
    case "double"     => Validation.Parsing.Value.Double
    case "float"      => Validation.Parsing.Value.Float
    case "instant"    => Validation.Parsing.Value.Instant
    case "int"        => Validation.Parsing.Value.Int
    case "long"       => Validation.Parsing.Value.Long
    case "short"      => Validation.Parsing.Value.Short
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

  implicit val decoderInspectorValidationError: Decoder[Validation.Error] = Decoder.instance { cursor =>
    (cursor.get[String](Keys.Type), cursor.get[Option[String]](Keys.Variant)).tupled.flatMap {
      // format: off
      case (Types.Collection, Some(Variants.AtLeast)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Collection.AtLeast(equal = false, reference, actual) }
      case (Types.Collection, Some(Variants.AtLeastEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Collection.AtLeast(equal = true, reference, actual) }
      case (Types.Collection, Some(Variants.AtMost)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Collection.AtMost(equal = false, reference, actual) }
      case (Types.Collection, Some(Variants.AtMostEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Collection.AtMost(equal = true, reference, actual) }
      case (Types.Collection, Some(Variants.Contains)) => decoder.reference[String, Seq[String]](cursor).map { case (reference, actual) => Validation.Error.Collection.Contains(reference, actual) }
      case (Types.Collection, Some(Variants.Exactly)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Collection.Exactly(reference,actual) }
      case (Types.Conflict, None) => decoder[String](cursor).map(Validation.Error.Conflict.apply)
      case (Types.Date, Some(Variants.After)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.After(equal = false, reference, actual) }
      case (Types.Date, Some(Variants.AfterEqual)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.After(equal = true, reference, actual) }
      case (Types.Date, Some(Variants.Before)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.Before(equal = false, reference, actual) }
      case (Types.Date, Some(Variants.BeforeEqual)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.Before(equal = true, reference, actual) }
      case (Types.Duration, Some(Variants.AtMost)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Error.Duration.AtMost(equal = false, reference, actual) }
      case (Types.Duration, Some(Variants.AtMostEqual)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Error.Duration.AtMost(equal = true, reference, actual) }
      case (Types.Duration, Some(Variants.AtLeast)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Error.Duration.AtLeast(equal = false, reference, actual) }
      case (Types.Duration, Some(Variants.AtLeastEqual)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Error.Duration.AtLeast(equal = true, reference, actual) }
      case (Types.Duration, Some(Variants.Exactly)) => decoder.reference[FiniteDuration, FiniteDuration](cursor).map { case (reference, actual) => Validation.Error.Duration.Exactly(reference, actual) }
      case (Types.Mapping, None) => (cursor.get[Option[Set[String]]](Keys.Reference), cursor.get[String](Keys.Actual)).mapN(Validation.Error.Mapping.apply)
      case (Types.Not, None) => cursor.get[Validation.Error](Keys.Error)
      case (Types.Number, Some(Variants.Equal)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.Equal(reference, actual) }
      case (Types.Number, Some(Variants.GreaterThan)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.GreaterThan(equal = false, reference, actual) }
      case (Types.Number, Some(Variants.GreaterThanEqual)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.GreaterThan(equal = true, reference, actual) }
      case (Types.Number, Some(Variants.LessThan)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.LessThan(equal = false, reference, actual) }
      case (Types.Number, Some(Variants.LessThanEqual)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.LessThan(equal = true, reference, actual) }
      case (Types.Parsing, Some(variant)) => (parsingValueFromString(variant).toRight(DecodingFailure("Invalid parsing variant", cursor.history)), decoder[String](cursor)).mapN(Validation.Error.Parsing.apply)
      case (Types.Required, None) => Validation.Error.Optional.Required.asRight
      case (Types.Text, Some(Variants.AtLeast)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtLeast(equal = false, reference, actual) }
      case (Types.Text, Some(Variants.AtLeastEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtLeast(equal = true, reference, actual) }
      case (Types.Text, Some(Variants.AtMost)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtMost(equal = false, reference, actual) }
      case (Types.Text, Some(Variants.AtMostEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtMost(equal = true, reference, actual) }
      case (Types.Text, Some(Variants.Email)) => decoder[String](cursor).map(Validation.Error.Text.Email.apply)
      case (Types.Text, Some(Variants.Equal)) => decoder.reference[String, String](cursor).map { case (reference, actual) => Validation.Error.Text.Equal(reference, actual) }
      case (Types.Text, Some(Variants.Exactly)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.Exactly(reference, actual) }
      case (Types.Text, Some(Variants.Matches)) => (cursor.get[String](Keys.Regex).map(new Regex(_)), decoder[String](cursor)).mapN(Validation.Error.Text.Matches.apply)
      case (Types.Unknown, None) => decoder[String](cursor).map(Validation.Error.Unknown.apply)
      case (tpe, variant) => DecodingFailure(s"Invalid validation error: type = $tpe, variant = $variant", cursor.history).asLeft
      // format: on
    }
  }

  implicit val encoderInspectorValidationError: Encoder.AsObject[Validation.Error] = Encoder.AsObject.instance {
    // format: off
    case Validation.Error.Collection.AtLeast(false, reference, actual) => encoder.reference(Types.Collection, Variants.AtLeast, reference, actual)
    case Validation.Error.Collection.AtLeast(true, reference, actual) => encoder.reference(Types.Collection, Variants.AtLeastEqual, reference, actual)
    case Validation.Error.Collection.AtMost(false, reference, actual) => encoder.reference(Types.Collection, Variants.AtMost, reference, actual)
    case Validation.Error.Collection.AtMost(true, reference, actual) => encoder.reference(Types.Collection, Variants.AtMostEqual, reference, actual)
    case Validation.Error.Collection.Contains(reference, actual) => encoder.reference(Types.Collection, Variants.Contains, reference, actual)
    case Validation.Error.Collection.Exactly(reference, actual) => encoder.reference(Types.Collection, Variants.Exactly, reference, actual)
    case Validation.Error.Conflict(actual) => encoder(Types.Conflict, actual)
    case Validation.Error.Date.After(false, reference, actual) => encoder.reference(Types.Date, Variants.After, reference, actual)
    case Validation.Error.Date.After(true, reference, actual) => encoder.reference(Types.Date, Variants.AfterEqual, reference, actual)
    case Validation.Error.Date.Before(false, reference, actual) => encoder.reference(Types.Date, Variants.Before, reference, actual)
    case Validation.Error.Date.Before(true, reference, actual) => encoder.reference(Types.Date, Variants.BeforeEqual, reference, actual)
    case Validation.Error.Duration.AtLeast(false, reference, actual) => encoder.reference(Types.Duration, Variants.AtLeast, reference, actual)
    case Validation.Error.Duration.AtLeast(true, reference, actual) => encoder.reference(Types.Duration, Variants.AtLeastEqual, reference, actual)
    case Validation.Error.Duration.AtMost(false, reference, actual) => encoder.reference(Types.Duration, Variants.AtMost, reference, actual)
    case Validation.Error.Duration.AtMost(true, reference, actual) => encoder.reference(Types.Duration, Variants.AtMostEqual, reference, actual)
    case Validation.Error.Duration.Exactly(reference, actual) => encoder.reference(Types.Duration, Variants.Exactly, reference, actual)
    case Validation.Error.Mapping(references, actual) => JsonObject(Keys.Type := Types.Mapping, Keys.Reference := references, Keys.Actual := actual)
    case Validation.Error.Not(error) => JsonObject(Keys.Type := Types.Not, Keys.Error := error.asJsonObject)
    case Validation.Error.Number.Equal(reference, actual) => encoder.reference(Types.Number, Variants.Equal, reference, actual)
    case Validation.Error.Number.GreaterThan(false, reference, actual) => encoder.reference(Types.Number, Variants.GreaterThan, reference, actual)
    case Validation.Error.Number.GreaterThan(true, reference, actual) => encoder.reference(Types.Number, Variants.GreaterThanEqual, reference, actual)
    case Validation.Error.Number.LessThan(false, reference, actual) => encoder.reference(Types.Number, Variants.LessThan, reference, actual)
    case Validation.Error.Number.LessThan(true, reference, actual) => encoder.reference(Types.Number, Variants.LessThanEqual, reference, actual)
    case Validation.Error.Optional.Required => JsonObject(Keys.Type := Types.Required)
    case Validation.Error.Parsing(reference, actual) => encoder.variant(Types.Parsing, parsingValueToString(reference), actual)
    case Validation.Error.Text.AtLeast(false, reference, actual) => encoder.reference(Types.Text, Variants.AtLeast, reference, actual)
    case Validation.Error.Text.AtLeast(true, reference, actual) => encoder.reference(Types.Text, Variants.AtLeastEqual, reference, actual)
    case Validation.Error.Text.AtMost(false, reference, actual) => encoder.reference(Types.Text, Variants.AtMost, reference, actual)
    case Validation.Error.Text.AtMost(true, reference, actual) => encoder.reference(Types.Text, Variants.AtMostEqual, reference, actual)
    case Validation.Error.Text.Email(actual) => encoder.variant(Types.Text, Variants.Email, actual)
    case Validation.Error.Text.Equal(reference, actual) => encoder.reference(Types.Text, Variants.Equal, reference, actual)
    case Validation.Error.Text.Exactly(reference, actual) => encoder.reference(Types.Text, Variants.Exactly, reference, actual)
    case Validation.Error.Text.Matches(regex, actual) => encoder.variant(Types.Text, Variants.Matches, actual) deepMerge JsonObject(Keys.Regex := regex.toString())
    case Validation.Error.Unknown(actual) => encoder(Types.Unknown, actual)
    // format: on
  }

  implicit val keyEncoderInspectorCursorHistory: KeyEncoder[List[CursorOp]] = KeyEncoder.instance(CursorOp.opsToPath)

  implicit val encoderInspectorValidatingDecoderError: Encoder[Either[String, NonEmptyList[Validation.Error]]] =
    Encoder.instance {
      case Right(errors) => Json.obj(Keys.Type := "errors", "errors" := errors)
      case Left(failure) => Json.obj(Keys.Type := "failure", "message" := failure)
    }

  implicit val keyEncoderInspectorSelectionHistory: KeyEncoder[Selection.History] = KeyEncoder.instance(_.toJsonPath)

  implicit val keyDecoderInspectorSelectionHistory: KeyDecoder[Selection.History] =
    KeyDecoder.instance(Selection.History.parse(_).toOption)

  implicit val decoderInspectorCursorErrors: Decoder[Validation.Errors] =
    Decoder[NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]]].map(Validation.Errors.apply)

  implicit val encoderInspectorCursorErrors: Encoder[Validation.Errors] =
    Encoder[NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]]].contramap(_.toNem)
}

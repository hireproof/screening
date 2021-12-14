package io.taig.inspector

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import io.circe._
import io.circe.syntax._

import java.time.Instant
import scala.util.matching.Regex

trait circe {
  private object Keys {
    val Actual = "actual"
    val Variant = "context"
    val Error = "error"
    val Reference = "reference"
    val Regex = "regex"
    val Type = "type"
  }

  private object Types {
    val Collection = "collection"
    val Conflict = "conflict"
    val Date = "date"
    val Not = "not"
    val Number = "number"
    val Parsing = "parsing"
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

  private val parsingValueToString: Validation.Parsing.Value => String = {
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

  implicit val decoderInspectorValidationError: Decoder[Validation.Error] = Decoder.instance { cursor =>
    (cursor.get[String](Keys.Variant), cursor.get[Option[String]](Keys.Type)).tupled.flatMap {
      // format: off
      case (Types.Collection, Some(Variants.AtLeast)) => decoder.reference[Int, Int](cursor).map(Validation.Error.Collection.AtLeast.tupled)
      case (Types.Collection, Some(Variants.AtMost)) => decoder.reference[Int, Int](cursor).map(Validation.Error.Collection.AtMost.tupled)
      case (Types.Collection, Some(Variants.Contains)) => decoder.reference[String, Seq[String]](cursor).map(Validation.Error.Collection.Contains.tupled)
      case (Types.Collection, Some(Variants.Exactly)) => decoder.reference[Int, Int](cursor).map(Validation.Error.Collection.Exactly.tupled)
      case (Types.Conflict, None) => decoder[String](cursor).map(Validation.Error.Conflict.apply)
      case (Types.Date, Some(Variants.After)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.After(equal = false, reference, actual) }
      case (Types.Date, Some(Variants.AfterEqual)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.After(equal = true, reference, actual) }
      case (Types.Date, Some(Variants.Before)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.Before(equal = false, reference, actual) }
      case (Types.Date, Some(Variants.BeforeEqual)) => decoder.reference[Instant, Instant](cursor).map { case (reference, actual) => Validation.Error.Date.Before(equal = true, reference, actual) }
      case (Types.Not, None) => cursor.get[Validation.Error](Keys.Error)
      case (Types.Number, Some(Variants.Equal)) => decoder.reference[Double, Double](cursor).map(Validation.Error.Number.Equal.tupled)
      case (Types.Number, Some(Variants.GreaterThan)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.GreaterThan(equal = false, reference, actual) }
      case (Types.Number, Some(Variants.GreaterThanEqual)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.GreaterThan(equal = true, reference, actual) }
      case (Types.Number, Some(Variants.LessThan)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.LessThan(equal = false, reference, actual) }
      case (Types.Number, Some(Variants.LessThanEqual)) => decoder.reference[Double, Double](cursor).map { case (reference, actual) => Validation.Error.Number.LessThan(equal = true, reference, actual) }
      case (Types.Parsing, Some(variant)) => (parsingValueFromString(variant).toRight(DecodingFailure("Invalid parsing variant", cursor.history)), decoder[String](cursor)).mapN(Validation.Error.Parsing.apply)
      case (Types.Text, Some(Variants.AtLeast)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtLeast(equal = false, reference, actual) }
      case (Types.Text, Some(Variants.AtLeastEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtLeast(equal = true, reference, actual) }
      case (Types.Text, Some(Variants.AtMost)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtMost(equal = false, reference, actual) }
      case (Types.Text, Some(Variants.AtMostEqual)) => decoder.reference[Int, Int](cursor).map { case (reference, actual) => Validation.Error.Text.AtMost(equal = true, reference, actual) }
      case (Types.Text, Some(Variants.Email)) => decoder[String](cursor).map(Validation.Error.Text.Email.apply)
      case (Types.Text, Some(Variants.Equal)) => decoder.reference[String, String](cursor).map(Validation.Error.Text.Equal.tupled)
      case (Types.Text, Some(Variants.Exactly)) => decoder.reference[Int, Int](cursor).map(Validation.Error.Text.Exactly.tupled)
      case (Types.Text, Some(Variants.Matches)) => (cursor.get[String](Keys.Regex).map(new Regex(_)), decoder[String](cursor)).mapN(Validation.Error.Text.Matches.apply)
      case (Types.Unknown, None) => decoder[String](cursor).map(Validation.Error.Unknown.apply)
      case (tpe, variant) => DecodingFailure(s"Invalid validation error: type = $tpe, variant = $variant", cursor.history).asLeft
      // format: on
    }
  }

  implicit val encoderInspectorValidationError: Encoder.AsObject[Validation.Error] = Encoder.AsObject.instance {
    // format: off
    case Validation.Error.Collection.AtLeast(reference, actual) => encoder.reference(Types.Collection, Variants.AtLeast, reference, actual)
    case Validation.Error.Collection.AtMost(reference, actual) => encoder.reference(Types.Collection, Variants.AtMost, reference, actual)
    case Validation.Error.Collection.Contains(reference, actual) => encoder.reference(Types.Collection, Variants.Contains, reference, actual)
    case Validation.Error.Collection.Exactly(reference, actual) => encoder.reference(Types.Collection, Variants.Exactly, reference, actual)
    case Validation.Error.Conflict(actual) => encoder(Types.Conflict, actual)
    case Validation.Error.Date.After(false, reference, actual) => encoder.reference(Types.Date, Variants.After, reference, actual)
    case Validation.Error.Date.After(true, reference, actual) => encoder.reference(Types.Date, Variants.AfterEqual, reference, actual)
    case Validation.Error.Date.Before(false, reference, actual) => encoder.reference(Types.Date, Variants.Before, reference, actual)
    case Validation.Error.Date.Before(true, reference, actual) => encoder.reference(Types.Date, Variants.BeforeEqual, reference, actual)
    case Validation.Error.Not(error) => JsonObject(Keys.Type := Types.Not, Keys.Error := error.asJsonObject)
    case Validation.Error.Number.Equal(reference, actual) => encoder.reference(Types.Number, Variants.Equal, reference, actual)
    case Validation.Error.Number.GreaterThan(false, reference, actual) => encoder.reference(Types.Number, Variants.GreaterThan, reference, actual)
    case Validation.Error.Number.GreaterThan(true, reference, actual) => encoder.reference(Types.Number, Variants.GreaterThanEqual, reference, actual)
    case Validation.Error.Number.LessThan(false, reference, actual) => encoder.reference(Types.Number, Variants.LessThan, reference, actual)
    case Validation.Error.Number.LessThan(true, reference, actual) => encoder.reference(Types.Number, Variants.LessThanEqual, reference, actual)
    case Validation.Error.Parsing(reference, actual) => encoder.variant(Types.Parsing, parsingValueToString(reference), actual)
    case Validation.Error.Text.AtLeast(false, reference, actual) => encoder.reference(Types.Text, Variants.AtLeast, reference, actual)
    case Validation.Error.Text.AtLeast(true, reference, actual) => encoder.reference(Types.Text, Variants.AtLeastEqual, reference, actual)
    case Validation.Error.Text.AtMost(false, reference, actual) => encoder.reference(Types.Text, Variants.AtMost, reference, actual)
    case Validation.Error.Text.AtMost(true, reference, actual) => encoder.reference(Types.Text, Variants.AtMostEqual, reference, actual)
    case Validation.Error.Text.Email(actual) => encoder.variant(Types.Text, Variants.Email, actual)
    case Validation.Error.Text.Equal(reference, actual) => encoder.reference(Types.Text, Variants.Equal, reference, actual)
    case Validation.Error.Text.Exactly(reference, actual) => encoder.reference(Types.Text, Variants.Email, reference, actual)
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

  implicit val encoderInspectorValidatingDecoderErrors: Encoder[ValidatingDecoder.Errors] =
    Encoder[Map[List[CursorOp], Either[String, NonEmptyList[Validation.Error]]]].contramap(_.toMap)

  implicit val keyEncoderInspectorSelectionHistory: KeyEncoder[Selection.History] = KeyEncoder.instance(_.toJsonPath)

  implicit val keyDecoderInspectorSelectionHistory: KeyDecoder[Selection.History] =
    KeyDecoder.instance(Selection.History.parse(_).toOption)

  implicit val decoderInspectorValidationGroupErrors: Decoder[ValidationGroup.Errors] =
    Decoder[Map[Selection.History, NonEmptyList[Validation.Error]]].emap { errors =>
      Either.cond(errors.nonEmpty, ValidationGroup.Errors.unsafeFromMap(errors), "NonEmptyMap")
    }

  implicit val encoderInspectorValidationGroupErrors: Encoder[ValidationGroup.Errors] =
    Encoder[Map[Selection.History, NonEmptyList[Validation.Error]]].contramap(_.toMap)
}

object circe extends circe {
  implicit final class RichCursor(val cursor: ACursor) extends AnyVal {
    def validate[A](implicit decoder: ValidatingDecoder[A]): ValidatingDecoder.Result[A] = decoder.tryDecode(cursor)

    def validateWith[I, O](validation: Validation[I, O])(implicit decoder: Decoder[I]): ValidatingDecoder.Result[O] = {
      decoder.tryDecode(cursor) match {
        case Right(value) =>
          validation.run(value).leftMap(ValidatingDecoder.Errors.fromErrors(cursor.history, _))
        case Left(failure) => Validated.invalid(ValidatingDecoder.Errors.fromDecodingFailure(failure))
      }
    }
  }
}

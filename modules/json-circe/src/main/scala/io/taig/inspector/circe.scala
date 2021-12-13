package io.taig.inspector

import cats.data.{NonEmptyList, Validated}
import io.circe._
import io.circe.syntax._

import java.time.temporal.Temporal

trait circe {
  private object Keys {
    val Actual = "actual"
    val Context = "context"
    val Error = "error"
    val Expected = "expected"
    val Reference = "reference"
    val Regex = "regex"
    val Type = "type"
  }

  private object Contexts {
    val Collection = "collection"
    val Date = "date"
    val Number = "number"
    val Parsing = "parsing"
    val Text = "text"
  }

  private object Types {
    val After = "after"
    val AtLeast = "atLeast"
    val AtLeastEqual = "atLeastEqual"
    val AtMost = "atMost"
    val AtMostEqual = "atMostEqual"
    val Before = "before"
    val Contains = "contains"
    val Email = "email"
    val Equal = "equal"
    val Exactly = "exactly"
    val GreaterThan = "greaterThan"
    val GreaterThanEqual = "greaterThanEqual"
    val LessThan = "lessThan"
    val LessThanEqual = "lessThanEqual"
    val Matches = "matches"
    val Not = "not"
  }

  private object encoder {
    def apply[A: Encoder](context: String, tpe: String, actual: A): JsonObject =
      JsonObject(Keys.Context := context, Keys.Type := tpe, Keys.Actual := actual)

    def expected[A: Encoder, B: Encoder](context: String, tpe: String, expected: A, actual: B): JsonObject =
      encoder(context, tpe, actual) deepMerge JsonObject(Keys.Expected := expected)

    def reference[A: Encoder, B: Encoder](context: String, tpe: String, reference: A, actual: B): JsonObject =
      encoder(context, tpe, actual) deepMerge JsonObject(Keys.Reference := reference)
  }

  implicit private val encoderTemporal: Encoder[Temporal] = Encoder[String].contramap(_.toString)

  private val parsingValueToString: Validation.Parsing.Value => String = {
    case Validation.Parsing.Value.BigDecimal => "bigDecimal"
    case Validation.Parsing.Value.BigInt     => "bigInt"
    case Validation.Parsing.Value.Double     => "double"
    case Validation.Parsing.Value.Float      => "float"
    case Validation.Parsing.Value.Int        => "int"
    case Validation.Parsing.Value.Long       => "long"
    case Validation.Parsing.Value.Short      => "short"
  }

  implicit val encoderInspectorValidationError: Encoder.AsObject[Validation.Error] = Encoder.AsObject.instance {
    case Validation.Error.Collection.AtLeast(expected, actual) =>
      encoder.expected(Contexts.Collection, Types.AtLeast, expected, actual)
    case Validation.Error.Collection.AtMost(expected, actual) =>
      encoder.expected(Contexts.Collection, Types.AtMost, expected, actual)
    case Validation.Error.Collection.Contains(reference, actual) =>
      encoder.reference(Contexts.Collection, Types.Contains, reference, actual)
    case Validation.Error.Collection.Exactly(expected, actual) =>
      encoder.expected(Contexts.Collection, Types.Exactly, expected, actual)
    case Validation.Error.Date.After(reference, actual) =>
      encoder.reference(Contexts.Date, Types.After, reference, actual)
    case Validation.Error.Date.Before(reference, actual) =>
      encoder.reference(Contexts.Date, Types.Before, reference, actual)
    case Validation.Error.Number.Equal(expected, actual) =>
      encoder.expected(Contexts.Number, Types.Equal, expected, actual)
    case Validation.Error.Number.GreaterThan(false, reference, actual) =>
      encoder.reference(Contexts.Number, Types.GreaterThan, reference, actual)
    case Validation.Error.Number.GreaterThan(true, reference, actual) =>
      encoder.reference(Contexts.Number, Types.GreaterThanEqual, reference, actual)
    case Validation.Error.Number.LessThan(false, reference, actual) =>
      encoder.reference(Contexts.Number, Types.LessThan, reference, actual)
    case Validation.Error.Number.LessThan(true, reference, actual) =>
      encoder.reference(Contexts.Number, Types.LessThanEqual, reference, actual)
    case Validation.Error.Parsing(expected, actual) => encoder(Contexts.Parsing, parsingValueToString(expected), actual)
    case Validation.Error.Text.AtLeast(false, expected, actual) =>
      encoder.expected(Contexts.Text, Types.AtLeast, expected, actual)
    case Validation.Error.Text.AtLeast(true, expected, actual) =>
      encoder.expected(Contexts.Text, Types.AtLeastEqual, expected, actual)
    case Validation.Error.Text.AtMost(false, expected, actual) =>
      encoder.expected(Contexts.Text, Types.AtMost, expected, actual)
    case Validation.Error.Text.AtMost(true, expected, actual) =>
      encoder.expected(Contexts.Text, Types.AtMostEqual, expected, actual)
    case Validation.Error.Text.Email(actual)           => encoder(Contexts.Text, Types.Email, actual)
    case Validation.Error.Text.Equal(expected, actual) => encoder.expected(Contexts.Text, Types.Equal, expected, actual)
    case Validation.Error.Text.Exactly(expected, actual) =>
      encoder.expected(Contexts.Text, Types.Email, expected, actual)
    case Validation.Error.Text.Matches(regex, actual) =>
      encoder(Contexts.Text, Types.Matches, actual) deepMerge JsonObject(Keys.Regex := regex.toString())
    case Validation.Error.Not(error) => JsonObject(Keys.Type := Types.Not, Keys.Error := error.asJsonObject)
  }

  implicit val keyEncoderInspectorCursorHistory: KeyEncoder[List[CursorOp]] = KeyEncoder.instance(CursorOp.opsToPath)

  implicit val encoderInspectorValidatingDecoderError: Encoder[Either[String, NonEmptyList[Validation.Error]]] =
    Encoder.instance {
      case Right(errors) => Json.obj(Keys.Type := "errors", "errors" := errors)
      case Left(failure) => Json.obj(Keys.Type := "failure", "message" := failure)
    }

  implicit val encoderInspectorValidatingDecoderErrors: Encoder[ValidatingDecoder.Errors] =
    Encoder[Map[List[CursorOp], Either[String, NonEmptyList[Validation.Error]]]].contramap(_.toMap)
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

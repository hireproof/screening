package io.taig.inspector

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{ACursor, CursorOp, Decoder, Encoder, FailedCursor, HCursor, JsonObject, KeyEncoder}
import io.taig.inspector.ValidatingDecoder.Result

import java.time.temporal.Temporal

trait circe {
  private object Keys {
    val Context = "context"
    val Error = "error"
    val Expected = "expected"
    val Actual = "actual"
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

  private val parsingValueToString: Validation.Rule.Parsing.Value => String = {
    case Validation.Rule.Parsing.Value.BigDecimal => "bigDecimal"
    case Validation.Rule.Parsing.Value.BigInt     => "bigInt"
    case Validation.Rule.Parsing.Value.Double     => "double"
    case Validation.Rule.Parsing.Value.Float      => "float"
    case Validation.Rule.Parsing.Value.Int        => "int"
    case Validation.Rule.Parsing.Value.Long       => "long"
    case Validation.Rule.Parsing.Value.Short      => "short"
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

  implicit val keyEncoderInspectorCursor: KeyEncoder[Selection.History] = KeyEncoder.instance { cursor =>
    cursor.values.foldLeft(".") {
      case (result, Selection.Field(name))  => s"$result.$name"
      case (result, Selection.Index(index)) => s"$result[$index]"
    }
  }

  implicit val encoderInspectorValidationErrors: Encoder[Validation.Errors] =
    Encoder[NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]]].contramap(_.values)
}

object circe extends circe {
  def toSelectionHistory(values: List[CursorOp]): Selection.History = Selection.History.from {
    values.foldRight(List.empty[Selection]) {
      case (CursorOp.DownField(name), selections)           => Selection.Field(name) :: selections
      case (CursorOp.DownN(index), selections)              => Selection.Index(index) :: selections
      case (CursorOp.DownArray, selections)                 => Selection.Index(0) :: selections
      case (CursorOp.MoveUp, _ :: tail)                     => tail
      case (CursorOp.MoveUp, selections)                    => selections
      case (CursorOp.MoveRight, Selection.Index(i) :: tail) => Selection.Index(i + 1) :: tail
      case (CursorOp.MoveRight, selections)                 => Selection.Index(0) :: selections
      case (CursorOp.MoveLeft, Selection.Index(i) :: tail)  => Selection.Index(i - 1) :: tail
      case (CursorOp.MoveLeft, selections)                  => Selection.Index(0) :: selections
      case (CursorOp.DeleteGoParent, _ :: tail)             => tail
      case (CursorOp.DeleteGoParent, selections)            => selections
      case (CursorOp.Field(name), _ :: tail)                => Selection.Field(name) :: tail
      case (CursorOp.Field(name), selections)               => Selection.Field(name) :: selections
    }
  }

  implicit final class RichCursor(val cursor: ACursor) extends AnyVal {
    def validate[A](implicit decoder: ValidatingDecoder[A]): ValidatingDecoder.Result[A] = cursor match {
      case cursor: HCursor => decoder.apply(cursor)
      case _               => ???
    }

    def validateWith[I, O](
        validation: Validation.Rule[I, O]
    )(implicit decoder: Decoder[I]): ValidatingDecoder.Result[O] = cursor match {
      case cursor: HCursor =>
        decoder.apply(cursor) match {
          case Right(input) =>
            validation.run(input).leftMap(ValidatingDecoder.Errors.one(toSelectionHistory(cursor.history), _))
          case Left(failure) => ???
        }
      case _ => ???
    }
  }
}

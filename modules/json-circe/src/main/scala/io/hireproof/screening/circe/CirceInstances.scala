package io.hireproof.screening.circe

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe.{Error => _, _}
import io.circe.syntax._
import io.hireproof.screening.{Constraint, Error, Selection, Violations}

import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

trait CirceInstances {
  private object Keys {
    val Actual = "actual"
    val Equal = "equal"
    val Delta = "delta"
    val Variant = "variant"
    val Left = "left"
    val Right = "right"
    val Reference = "reference"
    val Regex = "regex"
    val Type = "type"
  }

  private object Types {
    val Collection = "collection"
    val Conflict = "conflict"
    val Optional = "optional"
    val Time = "time"
    val Duration = "duration"
    val Invalid = "invalid"
    val Missing = "missing"
    val Number = "number"
    val OneOf = "oneOf"
    val Or = "or"
    val Parsing = "parsing"
    val Text = "text"
    val Unknown = "unknown"
  }

  private object Variants {
    val After = "after"
    val AtLeast = "atLeast"
    val AtMost = "atMost"
    val Before = "before"
    val Contains = "contains"
    val Equal = "equal"
    val Exactly = "exactly"
    val GreaterThan = "greaterThan"
    val IsDefined = "isDefined"
    val LessThan = "lessThan"
    val Matches = "matches"
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

  implicit private val encoderDuration: Encoder.AsObject[FiniteDuration] = Encoder.AsObject.instance { duration =>
    JsonObject("length" := duration.length, "unit" := duration.unit.name())
  }

  implicit val decoderConstraint: Decoder[Constraint] = Decoder.instance { cursor =>
    (cursor.get[String](Keys.Type), cursor.get[Option[String]](Keys.Variant)).tupled.flatMap {
      // format: off
      case (Types.Collection, Some(Variants.AtLeast)) => (cursor.get[Boolean](Keys.Equal), cursor.get[Long](Keys.Reference)).mapN(Constraint.Collection.AtLeast)
      case (Types.Collection, Some(Variants.AtMost)) => (cursor.get[Boolean](Keys.Equal), cursor.get[Long](Keys.Reference)).mapN(Constraint.Collection.AtMost)
      case (Types.Collection, Some(Variants.Contains)) => cursor.get[String](Keys.Reference).map(Constraint.Collection.Contains)
      case (Types.Collection, Some(Variants.Exactly)) => cursor.get[Long](Keys.Reference).map(Constraint.Collection.Exactly)
      case (Types.Duration, Some(Variants.AtLeast)) => (cursor.get[Boolean](Keys.Equal), cursor.get[FiniteDuration](Keys.Reference)).mapN(Constraint.Duration.AtLeast)
      case (Types.Duration, Some(Variants.AtMost)) => (cursor.get[Boolean](Keys.Equal), cursor.get[FiniteDuration](Keys.Reference)).mapN(Constraint.Duration.AtMost)
      case (Types.Duration, Some(Variants.Exactly)) => cursor.get[FiniteDuration](Keys.Reference).map(Constraint.Duration.Exactly)
      case (Types.Number, Some(Variants.Equal)) => (cursor.get[Double](Keys.Reference), cursor.get[Double](Keys.Delta)).mapN(Constraint.Number.Equal)
      case (Types.Number, Some(Variants.GreaterThan)) => (cursor.get[Boolean](Keys.Equal), cursor.get[Double](Keys.Reference), cursor.get[Double](Keys.Delta)).mapN(Constraint.Number.GreaterThan)
      case (Types.Number, Some(Variants.LessThan)) => (cursor.get[Boolean](Keys.Equal), cursor.get[Double](Keys.Reference), cursor.get[Double](Keys.Delta)).mapN(Constraint.Number.LessThan)
      case (Types.Optional, Some(Variants.IsDefined)) => Constraint.Optional.IsDefined.asRight
      case (Types.OneOf, None) => cursor.get[Set[String]](Keys.Reference).map(Constraint.OneOf)
      case (Types.Or, None) => (cursor.get[Set[Constraint]](Keys.Left), cursor.get[Set[Constraint]](Keys.Right)).mapN(Constraint.Or)
      case (Types.Parsing, None) => cursor.get[String](Keys.Reference).map(Constraint.Parsing)
      case (Types.Text, Some(Variants.AtLeast)) => (cursor.get[Boolean](Keys.Equal), cursor.get[Int](Keys.Reference)).mapN(Constraint.Text.AtLeast)
      case (Types.Text, Some(Variants.AtMost)) => (cursor.get[Boolean](Keys.Equal), cursor.get[Int](Keys.Reference)).mapN(Constraint.Text.AtMost)
      case (Types.Text, Some(Variants.Equal))   => cursor.get[String](Keys.Reference).map(Constraint.Text.Equal)
      case (Types.Text, Some(Variants.Exactly)) => cursor.get[Int](Keys.Reference).map(Constraint.Text.Exactly)
      case (Types.Text, Some(Variants.Matches)) => cursor.get[String](Keys.Regex).map(new Regex(_)).map(Constraint.Text.Matches)
      case (Types.Time, Some(Variants.After)) => (cursor.get[Boolean](Keys.Equal), cursor.get[ZonedDateTime](Keys.Reference)).mapN(Constraint.Time.After)
      case (Types.Time, Some(Variants.Before)) => (cursor.get[Boolean](Keys.Equal), cursor.get[ZonedDateTime](Keys.Reference)).mapN(Constraint.Time.Before)
      case (tpe, variant) => DecodingFailure(s"Invalid constraint: type = $tpe, variant = $variant", cursor.history).asLeft
      // format: on
    }
  }

  implicit val encoderConstraint: Encoder.AsObject[Constraint] = Encoder.AsObject.instance {
    // format: off
    case Constraint.Collection.AtLeast(equal, reference) => JsonObject(Keys.Type := Types.Collection, Keys.Variant := Variants.AtLeast, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Collection.AtMost(equal, reference) => JsonObject(Keys.Type := Types.Collection, Keys.Variant := Variants.AtMost, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Collection.Contains(reference) => JsonObject(Keys.Type := Types.Collection, Keys.Variant := Variants.Contains, Keys.Reference := reference)
    case Constraint.Collection.Exactly(reference) => JsonObject(Keys.Type := Types.Collection, Keys.Variant := Variants.Exactly, Keys.Reference := reference)
    case Constraint.Duration.AtLeast(equal, reference) => JsonObject(Keys.Type := Types.Duration, Keys.Variant := Variants.AtLeast, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Duration.AtMost(equal, reference) => JsonObject(Keys.Type := Types.Duration, Keys.Variant := Variants.AtMost, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Duration.Exactly(reference) => JsonObject(Keys.Type := Types.Duration, Keys.Variant := Variants.Exactly, Keys.Reference := reference)
    case Constraint.Number.Equal(reference, delta) => JsonObject(Keys.Type := Types.Number, Keys.Variant := Variants.Equal, Keys.Reference := reference, Keys.Delta := delta)
    case Constraint.Number.GreaterThan(equal, reference, delta) => JsonObject(Keys.Type := Types.Number, Keys.Variant := Variants.GreaterThan, Keys.Equal := equal, Keys.Reference := reference, Keys.Delta := delta)
    case Constraint.Number.LessThan(equal, reference, delta) => JsonObject(Keys.Type := Types.Number, Keys.Variant := Variants.LessThan, Keys.Equal := equal, Keys.Reference := reference, Keys.Delta := delta)
    case Constraint.Optional.IsDefined => JsonObject(Keys.Type := Types.Optional, Keys.Variant := Variants.IsDefined)
    case Constraint.OneOf(references) => JsonObject(Keys.Type := Types.OneOf, Keys.Reference := references)
    case Constraint.Or(left, right) => JsonObject(Keys.Type := Types.Or, Keys.Left := left, Keys.Right := right)
    case Constraint.Parsing(reference) => JsonObject(Keys.Type := Types.Parsing, Keys.Reference := reference)
    case Constraint.Text.AtLeast(equal, reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.AtLeast, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Text.AtMost(equal, reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.AtMost, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Text.Equal(reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.Equal, Keys.Reference := reference)
    case Constraint.Text.Exactly(reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.Exactly, Keys.Reference := reference)
    case Constraint.Text.Matches(reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.Matches, Keys.Regex := reference.regex)
    case Constraint.Time.After(equal, reference) => JsonObject(Keys.Type := Types.Time, Keys.Variant := Variants.After, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Time.Before(equal, reference) => JsonObject(Keys.Type := Types.Time, Keys.Variant := Variants.Before, Keys.Equal := equal, Keys.Reference := reference)
    // format: on
  }

  implicit val decoderError: Decoder[Error] = {
    def decode(json: Json): Any = json.fold(
      jsonNull = None,
      jsonBoolean = identity,
      jsonNumber = number => number.toInt.orElse(number.toLong).getOrElse(number.toDouble),
      jsonString = identity,
      jsonArray = _.map(decode).toList,
      jsonObject = identity
    )

    Decoder.instance { cursor =>
      cursor.get[String](Keys.Type).flatMap {
        case Types.Conflict => cursor.get[String](Keys.Actual).map(Error.Conflict)
        case Types.Invalid =>
          (cursor.get[Option[String]](Keys.Reference), cursor.get[String](Keys.Actual)).mapN(Error.Invalid)
        case Types.Missing => cursor.get[Option[String]](Keys.Reference).map(Error.Missing)
        case Types.Unknown => cursor.get[String](Keys.Actual).map(Error.Unknown)
        case _ => (cursor.as[Constraint], cursor.get[Json](Keys.Actual).map(decode)).mapN(Error.BrokenConstraint)
      }
    }
  }

  implicit val encoderError: Encoder.AsObject[Error] = {
    def encode(value: Any): Json = value match {
      case value: BigDecimal  => Json.fromBigDecimal(value)
      case value: Boolean     => Json.fromBoolean(value)
      case value: Double      => Json.fromDoubleOrString(value)
      case value: Float       => Json.fromFloatOrString(value)
      case value: Int         => Json.fromInt(value)
      case value: Long        => Json.fromLong(value)
      case value: Iterable[_] => value.map(encode).asJson
      case value: Option[_]   => value.map(encode).asJson
      case value              => Json.fromString(value.toString)
    }

    Encoder.AsObject.instance {
      case Error.BrokenConstraint(constraint, actual) =>
        constraint.asJsonObject deepMerge JsonObject(Keys.Actual := encode(actual))
      case Error.Conflict(actual) => JsonObject(Keys.Type := Types.Conflict, Keys.Actual := actual)
      case Error.Invalid(reference, actual) =>
        JsonObject(Keys.Type := Types.Invalid, Keys.Reference := reference, Keys.Actual := actual)
      case Error.Missing(reference) => JsonObject(Keys.Type := Types.Missing, Keys.Reference := reference)
      case Error.Unknown(actual) => JsonObject(Keys.Type := Types.Unknown, Keys.Actual := actual)
    }
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

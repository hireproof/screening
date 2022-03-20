package io.hireproof.screening.circe

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.hireproof.screening.{Constraint, Selection, Validation}

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
    val Error = "error"
    val Left = "left"
    val Right = "right"
    val Reference = "reference"
    val Regex = "regex"
    val Type = "type"
  }

  private object Types {
    val Collection = "collection"
    val Optional = "optional"
    val Conflict = "conflict"
    val Time = "time"
    val Duration = "duration"
    val Invalid = "invalid"
    val Mapping = "mapping"
    val Missing = "missing"
    val Not = "not"
    val Number = "number"
    val Or = "or"
    val Parsing = "parsing"
    val Required = "requires"
    val Text = "text"
    val Unknown = "unknown"
  }

  private object Variants {
    val After = "after"
    val AfterEqual = "afterEqual"
    val AtLeast = "atLeast"
    val AtMost = "atMost"
    val Before = "before"
    val Contains = "contains"
    val Email = "email"
    val Equal = "equal"
    val Exactly = "exactly"
    val GreaterThan = "greaterThan"
    val IsDefined = "isDefined"
    val LessThan = "lessThan"
    val Matches = "matches"
  }

  private object decoder {
    def apply[A: Decoder](cursor: HCursor): Either[DecodingFailure, A] = cursor.get[A](Keys.Actual)

    def reference[A: Decoder, B: Decoder](cursor: HCursor): Either[DecodingFailure, (A, B)] =
      (cursor.get[A](Keys.Reference), decoder[B](cursor)).tupled
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

  implicit val decoderConstraint: Decoder[Constraint] = Decoder.instance { cursor =>
    (cursor.get[String](Keys.Type), cursor.get[Option[String]](Keys.Variant)).tupled.flatMap {
      case (Types.Collection, Some(Variants.AtLeast)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[Long](Keys.Reference)).mapN(Constraint.Collection.AtLeast)
      case (Types.Collection, Some(Variants.AtMost)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[Long](Keys.Reference)).mapN(Constraint.Collection.AtMost)
      case (Types.Collection, Some(Variants.Contains)) =>
        cursor.get[String](Keys.Reference).map(Constraint.Collection.Contains)
      case (Types.Collection, Some(Variants.Exactly)) =>
        cursor.get[Long](Keys.Reference).map(Constraint.Collection.Exactly)
      case (Types.Duration, Some(Variants.AtLeast)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[FiniteDuration](Keys.Reference)).mapN(Constraint.Duration.AtLeast)
      case (Types.Duration, Some(Variants.AtMost)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[FiniteDuration](Keys.Reference)).mapN(Constraint.Duration.AtMost)
      case (Types.Duration, Some(Variants.Exactly)) =>
        cursor.get[FiniteDuration](Keys.Reference).map(Constraint.Duration.Exactly)
      case (Types.Number, Some(Variants.Equal)) =>
        (cursor.get[Double](Keys.Reference), cursor.get[Double](Keys.Delta)).mapN(Constraint.Number.Equal)
      case (Types.Number, Some(Variants.GreaterThan)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[Double](Keys.Reference), cursor.get[Double](Keys.Delta))
          .mapN(Constraint.Number.GreaterThan)
      case (Types.Number, Some(Variants.LessThan)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[Double](Keys.Reference), cursor.get[Double](Keys.Delta))
          .mapN(Constraint.Number.LessThan)
      case (Types.Optional, Some(Variants.IsDefined)) => Constraint.Optional.IsDefined.asRight
      case (Types.Or, None) =>
        (cursor.get[Set[Constraint]](Keys.Left), cursor.get[Set[Constraint]](Keys.Right)).mapN(Constraint.Or)
      case (Types.Parsing, None) => cursor.get[String](Keys.Reference).map(Constraint.Parsing)
      case (Types.Text, Some(Variants.AtLeast)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[Int](Keys.Reference)).mapN(Constraint.Text.AtLeast)
      case (Types.Text, Some(Variants.AtMost)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[Int](Keys.Reference)).mapN(Constraint.Text.AtMost)
      case (Types.Text, Some(Variants.Equal))   => cursor.get[String](Keys.Reference).map(Constraint.Text.Equal)
      case (Types.Text, Some(Variants.Exactly)) => cursor.get[Int](Keys.Reference).map(Constraint.Text.Exactly)
      case (Types.Text, Some(Variants.Matches)) =>
        cursor.get[String](Keys.Reference).map(new Regex(_)).map(Constraint.Text.Matches)
      case (Types.Time, Some(Variants.After)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[ZonedDateTime](Keys.Reference)).mapN(Constraint.Time.After)
      case (Types.Time, Some(Variants.Before)) =>
        (cursor.get[Boolean](Keys.Equal), cursor.get[ZonedDateTime](Keys.Reference)).mapN(Constraint.Time.Before)
      case (tpe, variant) =>
        DecodingFailure(s"Invalid constraint: type = $tpe, variant = $variant", cursor.history).asLeft
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
    case Constraint.Or(left, right) => JsonObject(Keys.Type := Types.Or, Keys.Left := left, Keys.Right := right)
    case Constraint.Parsing(reference) => JsonObject(Keys.Type := Types.Parsing, Keys.Reference := reference)
    case Constraint.Text.AtLeast(equal, reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.AtLeast, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Text.AtMost(equal, reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.AtMost, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Text.Equal(reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.Equal, Keys.Reference := reference)
    case Constraint.Text.Exactly(reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.Exactly, Keys.Reference := reference)
    case Constraint.Text.Matches(reference) => JsonObject(Keys.Type := Types.Text, Keys.Variant := Variants.Matches, Keys.Reference := reference.regex)
    case Constraint.Time.After(equal, reference) => JsonObject(Keys.Type := Types.Time, Keys.Variant := Variants.After, Keys.Equal := equal, Keys.Reference := reference)
    case Constraint.Time.Before(equal, reference) => JsonObject(Keys.Type := Types.Time, Keys.Variant := Variants.Before, Keys.Equal := equal, Keys.Reference := reference)
    // format: on
  }

  implicit val keyEncoderCursorHistory: KeyEncoder[List[CursorOp]] = KeyEncoder.instance(CursorOp.opsToPath)

  implicit val keyEncoderSelectionHistory: KeyEncoder[Selection.History] = KeyEncoder.instance(_.toJsonPath)

  implicit val keyDecoderSelectionHistory: KeyDecoder[Selection.History] =
    KeyDecoder.instance(Selection.History.parse(_).toOption)

  implicit val decoderValidationViolations: Decoder[Validation.Violations] =
    Decoder[NonEmptyMap[Selection.History, NonEmptyList[Constraint]]].map(Validation.Violations.apply)

  implicit val encoderValidationViolations: Encoder[Validation.Violations] =
    Encoder[NonEmptyMap[Selection.History, NonEmptyList[Constraint]]].contramap(_.toNem)
}

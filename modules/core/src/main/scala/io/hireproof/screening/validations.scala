package io.hireproof.screening

import cats.data.Chain
import cats.syntax.all._
import cats.{Eq, Traverse, UnorderedFoldable}
import io.circe.{Decoder, Encoder, Json, JsonObject}
import io.circe.parser.parse

import java.time._
import java.time.format.DateTimeParseException
import java.util.UUID
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.ClassTag
import scala.util.matching.Regex

object validations {
  abstract class iterable[F[a] <: Iterable[a]] {
    def size[A]: Validation[F[A], Int] = Validation.lift(_.size)

    def atLeast[A](reference: Int, equal: Boolean = true): Validation[F[A], Unit] =
      size.andThen(number.greaterThan(reference, equal, delta = 0))

    def atMost[A](reference: Int, equal: Boolean = true): Validation[F[A], Unit] =
      size.andThen(number.lessThan(reference, equal, delta = 0))

    def empty[A]: Validation[F[A], Unit] = atMost(reference = 0)

    def nonEmpty[A]: Validation[F[A], Unit] = atLeast(reference = 1)

    def exactly[A](reference: Int): Validation[F[A], Unit] = size.andThen(number.equal(reference))
  }

  abstract class seq[F[+a] <: Seq[a]] extends iterable[F] {
    def contains[A: Encoder](reference: A): Validation[F[A], Unit] =
      Validation.condNel[F[A]](Constraint.collection.contains(reference))(_.contains(reference))
  }

  object seq extends seq[Seq]
  object list extends seq[List]
  object vector extends seq[Vector]

  object set extends iterable[Set] {
    def contains[A: Encoder](reference: A): Validation[Set[A], Unit] =
      Validation.condNel(Constraint.collection.contains(reference))(_.contains(reference))
  }

  object foldable {
    def size[F[_]: UnorderedFoldable, A]: Validation[F[A], Long] = Validation.lift[F[A], Long](_.size)

    def atLeast[F[_]: UnorderedFoldable, A](reference: Long, equal: Boolean = true): Validation[F[A], Unit] =
      size[F, A].andThen(number.greaterThan(reference, equal, delta = 0L))

    def atMost[F[_]: UnorderedFoldable, A](reference: Long, equal: Boolean = true): Validation[F[A], Unit] =
      size[F, A].andThen(number.lessThan(reference, equal, delta = 0L))

    def empty[F[_]: UnorderedFoldable, A]: Validation[F[A], Unit] = atMost(reference = 0)

    def nonEmpty[F[_]: UnorderedFoldable, A]: Validation[F[A], Unit] = atLeast(reference = 1)

    def exactly[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] =
      size[F, A].andThen(number.equal(reference))
  }

  object traversable {
    def contains[F[_]: Traverse, A: Encoder: Eq](reference: A): Validation[F[A], Unit] = {
      implicit val encoder: Encoder[F[A]] = Encoder[List[A]].contramap(_.toList)
      Validation.condNel[F[A]](Constraint.collection.contains(reference))(_.contains_(reference))
    }
  }

  object duration {
    def greaterThan(reference: FiniteDuration, equal: Boolean = true): Validation[FiniteDuration, Unit] =
      Validation.condNel(Constraint.duration.greaterThan(reference, equal)) { input =>
        if (equal) input >= reference else input > reference
      }

    def lessThan(reference: FiniteDuration, equal: Boolean = true): Validation[FiniteDuration, Unit] =
      Validation.condNel(Constraint.duration.lessThan(reference, equal)) { input =>
        if (equal) input <= reference else input < reference
      }

    def equal(reference: FiniteDuration): Validation[FiniteDuration, Unit] =
      Validation.condNel(Constraint.duration.equal(reference))(_ == reference)
  }

  object json {
    def apply[A: Decoder](reference: String): Validation[Json, A] =
      Validation.fromOptionNel(Constraint.json(reference))(_.as[A].toOption)

    def field(name: String): Validation[Json, Json] =
      Validation.fromOptionNel(Constraint.json(reference = name))(_.hcursor.downField(name).focus)

    def index(i: Int): Validation[Json, Json] =
      Validation.fromOptionNel(Constraint.json(reference = String.valueOf(i)))(_.hcursor.downN(i).focus)

    val array: Validation[Json, Chain[Json]] =
      Validation.fromOptionNel(Constraint.json(reference = "[]"))(_.as[Chain[Json]].toOption)

    val obj: Validation[Json, JsonObject] =
      Validation.fromOptionNel(Constraint.json(reference = "{}"))(_.as[JsonObject].toOption)

    val string: Validation[Json, String] =
      Validation.fromOptionNel(Constraint.json(reference = "String"))(_.asString)
  }

  object number {
    def equal[I: Numeric: Encoder](reference: I, delta: I): Validation[I, Unit] =
      Validation.condNel[I](Constraint.number.equal(reference, delta.toDouble)) { input =>
        (reference - input).abs <= delta
      }

    def equal[I: Encoder](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      equal(reference, delta = numeric.zero)

    def greaterThan[I: Numeric: Encoder](reference: I, equal: Boolean, delta: I): Validation[I, Unit] =
      Validation.condNel[I](Constraint.number.greaterThan(reference, delta.toDouble, equal)) { input =>
        if (equal) input - reference >= -delta else input - reference > -delta
      }

    def greaterThanEqual[I: Numeric: Encoder](reference: I, delta: I): Validation[I, Unit] =
      greaterThan(reference, equal = true, delta)

    def greaterThanEqual[I: Encoder](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      greaterThanEqual(reference, delta = numeric.zero)

    def greaterThanNotEqual[I: Numeric: Encoder](reference: I, delta: I): Validation[I, Unit] =
      greaterThan(reference, equal = false, delta)

    def greaterThanNotEqual[I: Encoder](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      greaterThanNotEqual(reference, delta = numeric.zero)

    def lessThan[I: Numeric: Encoder](reference: I, equal: Boolean, delta: I): Validation[I, Unit] =
      Validation.condNel[I](Constraint.number.lessThan(reference, delta.toDouble, equal)) { input =>
        if (equal) reference - input >= -delta else reference - input > -delta
      }

    def lessThanEqual[I: Numeric: Encoder](reference: I, delta: I): Validation[I, Unit] =
      lessThan(reference, equal = true, delta)

    def lessThanEqual[I: Encoder](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      lessThanEqual(reference, delta = numeric.zero)

    def lessThanNotEqual[I: Numeric: Encoder](reference: I, delta: I): Validation[I, Unit] =
      lessThan(reference, equal = false, delta)

    def lessThanNotEqual[I: Encoder](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      lessThanNotEqual(reference, delta = numeric.zero)
  }

  def oneOf[I: Encoder](references: Set[I]): Validation[I, Unit] =
    Validation.condNel(Constraint.oneOf(references))(references.contains)

  object optional {
    def isDefined[A]: Validation[Option[A], A] = Validation.required
  }

  object parsing {
    def apply[O](name: String)(parse: String => Option[O]): Validation[String, O] =
      Validation.fromOptionNel(Constraint.parsing(name))(parse)

    def catchOnly[T >: Null <: Throwable]: CatchOnlyBuilder[T] = new CatchOnlyBuilder[T]

    final class CatchOnlyBuilder[T >: Null <: Throwable] {
      def apply[O](name: String)(parse: String => O)(implicit tag: ClassTag[T]): Validation[String, O] =
        Validation.catchOnly[T](Constraint.parsing(name))(parse)
    }

    val bigDecimal: Validation[String, BigDecimal] = catchOnly[NumberFormatException]("bigDecimal")(BigDecimal(_))
    val bigInt: Validation[String, BigInt] = catchOnly[NumberFormatException]("bigInt")(BigInt(_))
    val boolean: Validation[String, Boolean] = parsing("boolean")(_.toBooleanOption)
    val double: Validation[String, Double] = parsing("double")(_.toDoubleOption)
    val duration: Validation[String, Duration] = catchOnly[NumberFormatException]("duration")(Duration(_))
    val float: Validation[String, Float] = parsing("float")(_.toFloatOption)
    val instant: Validation[String, Instant] = catchOnly[DateTimeParseException]("instant")(Instant.parse)
    val int: Validation[String, Int] = parsing("int")(_.toIntOption)
    val localDate: Validation[String, LocalDate] = catchOnly[DateTimeParseException]("localDate")(LocalDate.parse)
    val localDateTime: Validation[String, LocalDateTime] =
      catchOnly[DateTimeParseException]("localDateTime")(LocalDateTime.parse)
    val localTime: Validation[String, LocalTime] = catchOnly[DateTimeParseException]("localTime")(LocalTime.parse)
    val long: Validation[String, Long] = parsing("long")(_.toLongOption)
    val offsetDateTime: Validation[String, OffsetDateTime] =
      catchOnly[DateTimeParseException]("offsetDateTime")(OffsetDateTime.parse)
    val offsetTime: Validation[String, OffsetTime] = catchOnly[DateTimeParseException]("offsetTime")(OffsetTime.parse)
    val json: Validation[String, Json] = parsing("json")(parse(_).toOption)
    val short: Validation[String, Short] = parsing("short")(_.toShortOption)
    val uuid: Validation[String, UUID] = catchOnly[IllegalArgumentException]("uuid")(UUID.fromString)
    val zonedDateTime: Validation[String, ZonedDateTime] =
      catchOnly[DateTimeParseException]("zonedDateTime")(ZonedDateTime.parse)
  }

  object text {
    val length: Validation[String, Int] = Validation.ask[String].map(_.length)

    val trim: Validation[String, String] = Validation.ask[String].map(_.trim)

    def atLeast(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      length.andThen(number.greaterThan(reference, equal, delta = 0))

    def atMost(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      length.andThen(number.lessThan(reference, equal, delta = 0))

    val email: Validation[String, Unit] = matches("""^.+@.+$""".r).withConstraint(Constraint.text.email)

    val empty: Validation[String, Unit] = atMost(reference = 0)

    def equal(reference: String): Validation[String, Unit] =
      Validation.condNel(Constraint.text.equal(reference))(_ == reference)

    val nonEmpty: Validation[String, Unit] = atLeast(reference = 1)

    def exactly(reference: Int): Validation[String, Unit] = length.andThen(number.equal(reference, delta = 0))

    def matches(regex: Regex): Validation[String, Unit] =
      Validation.condNel[String](Constraint.text.matches(regex))(regex.matches)

    val required: Validation[String, String] = trim.andThen(nonEmpty.tap)
  }

  object time {
    def after(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.condNel(Constraint.time.after(reference.atZone(ZoneOffset.UTC), equal)) { input =>
        val result = input.compareTo(reference)
        if (equal) result >= 0 else result > 0
      }

    def before(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.condNel(Constraint.time.before(reference.atZone(ZoneOffset.UTC), equal)) { input =>
        val result = input.compareTo(reference)
        if (equal) result <= 0 else result < 0
      }
  }
}

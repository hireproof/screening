package io.hireproof.screening

import cats.syntax.all._
import cats.{Eq, Show, Traverse, UnorderedFoldable}

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
    val size: Validation[F[_], Int] = Validation.lift(_.size)

    def atLeast[A](reference: Int, equal: Boolean = true): Validation[F[A], Unit] = size.andThen {
      Validation.condNel(Constraint.Collection.AtLeast(equal, reference.toLong)) { input =>
        if (equal) input >= reference else input > reference
      }
    }

    def atMost[A](reference: Int, equal: Boolean = true): Validation[F[A], Unit] = size.andThen {
      Validation.condNel(Constraint.Collection.AtMost(equal, reference.toLong)) { input =>
        if (equal) input <= reference else input < reference
      }
    }

    def empty[A]: Validation[F[A], Unit] = atMost(reference = 0)

    def nonEmpty[A]: Validation[F[A], Unit] = Validation.not(empty)

    def exactly[A](reference: Int): Validation[F[A], Unit] = size.andThen {
      Validation.condNel(Constraint.Collection.Exactly(reference.toLong))(_ == reference)
    }
  }

  abstract class seq[F[a] <: Seq[a]] extends iterable[F] {
    def contains[A: Show](reference: A): Validation[F[A], Unit] =
      Validation.condNel(Constraint.Collection.Contains(reference.show))(_.contains(reference))
  }

  object seq extends seq[Seq]
  object list extends seq[List]
  object vector extends seq[Vector]

  object set extends iterable[Set] {
    def contains[A: Show](reference: A): Validation[Set[A], Unit] =
      Validation.condNel(Constraint.Collection.Contains(reference.show))(_.contains(reference))
  }

  object foldable {
    def size[F[_]: UnorderedFoldable]: Validation[F[_], Long] = Validation.lift(_.size)

    def atLeast[F[_]: UnorderedFoldable, A](reference: Long, equal: Boolean = true): Validation[F[A], Unit] =
      size[F].andThen {
        Validation.condNel(Constraint.Collection.AtLeast(equal, reference)) { input =>
          if (equal) input >= reference else input > reference
        }
      }

    def atMost[F[_]: UnorderedFoldable, A](reference: Long, equal: Boolean = true): Validation[F[A], Unit] =
      size[F].andThen {
        Validation.condNel(Constraint.Collection.AtMost(equal, reference)) { input =>
          if (equal) input <= reference else input < reference
        }
      }

    def empty[F[_]: UnorderedFoldable, A]: Validation[F[A], Unit] = atMost(reference = 0)

    def nonEmpty[F[_]: UnorderedFoldable, A]: Validation[F[A], Unit] = Validation.not(empty)

    def exactly[F[_]: UnorderedFoldable, A](reference: Long): Validation[F[A], Unit] = size[F].andThen {
      Validation.condNel(Constraint.Collection.Exactly(reference))(_ == reference)
    }
  }

  object traversable {
    def contains[F[_]: Traverse, A: Eq: Show](reference: A): Validation[F[A], Unit] =
      Validation.condNel(Constraint.Collection.Contains(reference.show))(_.contains_(reference))
  }

  object duration {
    def atLeast(reference: FiniteDuration, equal: Boolean = true): Validation[FiniteDuration, Unit] =
      Validation.condNel(Constraint.Duration.AtLeast(equal, reference)) { input =>
        if (equal) input >= reference else input > reference
      }

    def atMost(reference: FiniteDuration, equal: Boolean = true): Validation[FiniteDuration, Unit] =
      Validation.condNel(Constraint.Duration.AtMost(equal, reference)) { input =>
        if (equal) input <= reference else input < reference
      }

    def exactly(reference: FiniteDuration): Validation[FiniteDuration, Unit] =
      Validation.condNel(Constraint.Duration.Exactly(reference))(_ == reference)
  }
//
//  object mapping {
//    def apply[I: Show, O](
//        f: I => Option[O],
//        references: Option[Set[I]] = None
//    ): Validation[I, O] = Validation.Mapping(f, references)
//
//    def partial[I: Show, O](
//        pf: PartialFunction[I, O],
//        references: Option[Set[I]] = None
//    ): Validation[I, O] = Validation.Mapping(pf.lift, references)
//  }

  object number {
    def equal[I: Numeric](reference: I, delta: I): Validation[I, Unit] =
      Validation.condNel(Constraint.Number.Equal(reference.toDouble, delta.toDouble)) { input =>
        input == reference
      }

    def equal[I](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      equal(reference, delta = numeric.zero)

    def greaterThan[I: Numeric](reference: I, equal: Boolean, delta: I): Validation[I, Unit] =
      Validation.condNel(Constraint.Number.GreaterThan(equal, reference.toDouble, delta.toDouble)) { input =>
        if (equal) input <= reference else input < reference
      }

    def greaterThanEqual[I](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      greaterThan(reference, equal = true, delta = numeric.zero)

    def greaterThanNotEqual[I](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      greaterThan(reference, equal = false, delta = numeric.zero)

    def lessThan[I: Numeric](reference: I, equal: Boolean, delta: I): Validation[I, Unit] =
      Validation.condNel(Constraint.Number.GreaterThan(equal, reference.toDouble, delta.toDouble)) { input =>
        if (equal) input <= reference else input < reference
      }

    def lessThanEqual[I](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      lessThan(reference, equal = true, delta = numeric.zero)

    def lessThanNotEqual[I](reference: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      lessThan(reference, equal = false, delta = numeric.zero)
  }

  object optional {
    def isDefined[A]: Validation[Option[A], A] = Validation.fromOptionNel(Constraint.Optional.IsDefined)(identity)
  }

  object parsing {
    def apply[O](reference: String)(parse: String => Option[O]): Validation[String, O] =
      Validation.fromOptionNel(Constraint.Parsing(reference))(parse)

    def catchOnly[T >: Null <: Throwable]: CatchOnlyBuilder[T] = new CatchOnlyBuilder[T]

    final class CatchOnlyBuilder[T >: Null <: Throwable] {
      def apply[O](reference: String)(parse: String => O)(implicit tag: ClassTag[T]): Validation[String, O] =
        Validation.catchOnly[T](Constraint.Parsing(reference))(parse)
    }

    val bigDecimal: Validation[String, BigDecimal] = catchOnly[NumberFormatException]("BigDecimal")(BigDecimal(_))
    val bigInt: Validation[String, BigInt] = catchOnly[NumberFormatException]("BigInt")(BigInt(_))
    val boolean: Validation[String, Boolean] = parsing("Boolean")(_.toBooleanOption)
    val double: Validation[String, Double] = parsing("Double")(_.toDoubleOption)
    val duration: Validation[String, Duration] = catchOnly[NumberFormatException]("Duration")(Duration(_))
    val float: Validation[String, Float] = parsing("Float")(_.toFloatOption)
    val instant: Validation[String, Instant] = catchOnly[DateTimeParseException]("Instant")(Instant.parse)
    val int: Validation[String, Int] = parsing("Int")(_.toIntOption)
    val localDate: Validation[String, LocalDate] = catchOnly[DateTimeParseException]("LocalDate")(LocalDate.parse)
    val localDateTime: Validation[String, LocalDateTime] =
      catchOnly[DateTimeParseException]("LocalDateTime")(LocalDateTime.parse)
    val localTime: Validation[String, LocalTime] = catchOnly[DateTimeParseException]("LocalTime")(LocalTime.parse)
    val long: Validation[String, Long] = parsing("Long")(_.toLongOption)
    val offsetDateTime: Validation[String, OffsetDateTime] =
      catchOnly[DateTimeParseException]("OffsetDateTime")(OffsetDateTime.parse)
    val offsetTime: Validation[String, OffsetTime] = catchOnly[DateTimeParseException]("OffsetTime")(OffsetTime.parse)
    val short: Validation[String, Short] = parsing("Short")(_.toShortOption)
    val uuid: Validation[String, UUID] = catchOnly[IllegalArgumentException]("UUID")(UUID.fromString)
    val zonedDateTime: Validation[String, ZonedDateTime] =
      catchOnly[DateTimeParseException]("ZonedDateTime")(ZonedDateTime.parse)
  }

  object text {
    val length: Validation[String, Int] = Validation.ask[String].map(_.length)

    val trim: Validation[String, String] = Validation.ask[String].map(_.trim)

    def atLeast(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      length.andThen(Validation.condNel(Constraint.Text.AtLeast(equal, reference)) { input =>
        if (equal) input >= reference else input > reference
      })

    def atMost(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      length.andThen(Validation.condNel(Constraint.Text.AtMost(equal, reference)) { input =>
        if (equal) input <= reference else input < reference
      })

    val empty: Validation[String, Unit] = atMost(reference = 0)

    def equal(reference: String): Validation[String, Unit] =
      Validation.condNel(Constraint.Text.Equal(reference))(_ == reference)

    val nonEmpty: Validation[String, Unit] = Validation.not(empty)

    def exactly(reference: Int): Validation[String, Unit] =
      length.andThen(Validation.condNel(Constraint.Text.Exactly(reference))(_ == reference))

    def matches(regex: Regex): Validation[String, Unit] =
      Validation.condNel(Constraint.Text.Matches(regex))(regex.matches)

    val required: Validation[String, String] = trim.andThen(nonEmpty.tap)
  }

  object time {
    def after(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.condNel(Constraint.Time.After(equal, reference.atZone(ZoneOffset.UTC))) { input =>
        val result = input.compareTo(reference)
        if (equal) result >= 0 else result > 0
      }

    def before(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.condNel(Constraint.Time.After(equal, reference.atZone(ZoneOffset.UTC))) { input =>
        val result = input.compareTo(reference)
        if (equal) result <= 0 else result < 0
      }
  }
}

package io.taig.inspector

import cats.{Eq, Show}

import java.time.{
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
  Year,
  YearMonth,
  ZonedDateTime
}
import scala.util.matching.Regex

object rule {
  def lift[I, O](f: I => O): Validation.Rule[I, O] = Validation.Rule.Lift(f)

  trait collection {
    def atLeast(reference: Int): Validation.Rule[Iterable[_], Unit] = Validation.Rule.Collection.AtLeast(reference)

    def atMost(reference: Int): Validation.Rule[Iterable[_], Unit] = Validation.Rule.Collection.AtMost(reference)

    def contains[A: Eq: Show](reference: A): Validation.Rule[Seq[A], Unit] =
      Validation.Rule.Collection.Contains(reference)

    val empty: Validation.Rule[Iterable[_], Unit] = atMost(reference = 0)

    val nonEmpty: Validation.Rule[Iterable[_], Unit] = Validation.Rule.Not(empty)

    def exactly(expected: Int): Validation.Rule[Iterable[_], Unit] =
      (atLeast(expected) and atMost(expected)).modifyError {
        case Validation.Error.Collection.AtLeast(reference, actual) =>
          Validation.Error.Collection.Exactly(reference, actual)
        case Validation.Error.Collection.AtMost(reference, actual) =>
          Validation.Error.Collection.Exactly(reference, actual)
      }
  }

  object collection extends collection

  trait date {
    def after(reference: Instant): Validation.Rule[Instant, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: Instant): Validation.Rule[Instant, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: OffsetDateTime): Validation.Rule[OffsetDateTime, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: OffsetDateTime): Validation.Rule[OffsetDateTime, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: LocalDateTime): Validation.Rule[LocalDateTime, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: LocalDateTime): Validation.Rule[LocalDateTime, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: OffsetTime): Validation.Rule[OffsetTime, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: OffsetTime): Validation.Rule[OffsetTime, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: LocalTime): Validation.Rule[LocalTime, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: LocalTime): Validation.Rule[LocalTime, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: Year): Validation.Rule[Year, Unit] = Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: Year): Validation.Rule[Year, Unit] = Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: YearMonth): Validation.Rule[YearMonth, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: YearMonth): Validation.Rule[YearMonth, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: LocalDate): Validation.Rule[LocalDate, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: LocalDate): Validation.Rule[LocalDate, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)

    def after(reference: ZonedDateTime): Validation.Rule[ZonedDateTime, Unit] =
      Validation.Rule.Date.After(reference)(_ isAfter _)

    def before(reference: ZonedDateTime): Validation.Rule[ZonedDateTime, Unit] =
      Validation.Rule.Date.Before(reference)(_ isBefore _)
  }

  object date extends date

  trait number {
    def equal[I: Numeric](expected: I, delta: I): Validation.Rule[I, Unit] = Validation.Rule.Number(
      Validation.Rule.Number.Operator.Equal,
      expected,
      delta
    )

    def equal[I](expected: I)(implicit numeric: Numeric[I]): Validation.Rule[I, Unit] =
      equal(expected, delta = numeric.zero)

    def greaterThan[I](reference: I)(implicit numeric: Numeric[I]): Validation.Rule[I, Unit] = Validation.Rule.Number(
      Validation.Rule.Number.Operator.GreaterThan(equal = false),
      reference,
      numeric.zero
    )

    def greaterThanEqual[I](reference: I)(implicit numeric: Numeric[I]): Validation.Rule[I, Unit] =
      Validation.Rule.Number(
        Validation.Rule.Number.Operator.GreaterThan(equal = true),
        reference,
        numeric.zero
      )

    def lessThan[I](reference: I)(implicit numeric: Numeric[I]): Validation.Rule[I, Unit] = Validation.Rule.Number(
      Validation.Rule.Number.Operator.LessThan(equal = false),
      reference,
      numeric.zero
    )

    def lessThanEqual[I](reference: I)(implicit numeric: Numeric[I]): Validation.Rule[I, Unit] = Validation.Rule.Number(
      Validation.Rule.Number.Operator.LessThan(equal = true),
      reference,
      numeric.zero
    )
  }

  object number extends number

  trait parsing {
    val bigDecimal: Validation.Rule[String, BigDecimal] = Validation.Rule.Parsing.BigDecimal

    val bigInt: Validation.Rule[String, BigInt] = Validation.Rule.Parsing.BigInt

    val double: Validation.Rule[String, Double] = Validation.Rule.Parsing.Double

    val float: Validation.Rule[String, Float] = Validation.Rule.Parsing.Float

    val int: Validation.Rule[String, Int] = Validation.Rule.Parsing.Int

    val long: Validation.Rule[String, Long] = Validation.Rule.Parsing.Long

    val short: Validation.Rule[String, Short] = Validation.Rule.Parsing.Short
  }

  object parsing extends parsing

  trait text {
    val length: Validation.Rule[String, Int] = lift(_.length)

    val trim: Validation.Rule[String, String] = lift(_.trim)

    def atLeast(reference: Int): Validation.Rule[String, Unit] = Validation.Rule.Text.AtLeast(reference)

    def atMost(reference: Int): Validation.Rule[String, Unit] = Validation.Rule.Text.AtMost(reference)

    val email: Validation.Rule[String, Unit] = matches("""^.+@.+\..+$""".r).modifyError {
      case Validation.Error.Text.Matches(_, actual) => Validation.Error.Text.Email(actual)
    }

    val empty: Validation.Rule[String, Unit] = atMost(reference = 0)

    def equal(expected: String): Validation.Rule[String, Unit] = Validation.Rule.Text.Equal(expected)

    val nonEmpty: Validation.Rule[String, Unit] = Validation.Rule.Not(empty)

    def exactly(expected: Int): Validation.Rule[String, Unit] = (atLeast(expected) and atMost(expected)).modifyError {
      case Validation.Error.Text.AtLeast(reference, actual) => Validation.Error.Text.Exactly(reference, actual)
      case Validation.Error.Text.AtMost(reference, actual)  => Validation.Error.Text.Exactly(reference, actual)
    }

    def matches(regex: Regex): Validation.Rule[String, Unit] = Validation.Rule.Text.Matches(regex)

    val required: Validation.Rule[String, String] = trim.andThen(nonEmpty.tap)
  }

  object text extends text
}

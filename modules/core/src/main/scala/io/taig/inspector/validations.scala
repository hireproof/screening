package io.taig.inspector

import cats.{Eq, Show}

import java.time._
import scala.util.matching.Regex

object validations {
  def lift[I, O](f: I => O): Validation[I, O] = Validation.Lift(f)

  trait collection {
    def atLeast(reference: Int): Validation[Iterable[_], Unit] = Validation.Collection.AtLeast(reference)

    def atMost(reference: Int): Validation[Iterable[_], Unit] = Validation.Collection.AtMost(reference)

    def contains[A: Eq: Show](reference: A): Validation[Seq[A], Unit] =
      Validation.Collection.Contains(reference)

    val empty: Validation[Iterable[_], Unit] = atMost(reference = 0)

    val nonEmpty: Validation[Iterable[_], Unit] = Validation.Not(empty)

    def exactly(expected: Int): Validation[Iterable[_], Unit] = (atLeast(expected) and atMost(expected)).modifyError {
      case Validation.Error.Collection.AtLeast(reference, actual) =>
        Validation.Error.Collection.Exactly(reference, actual)
      case Validation.Error.Collection.AtMost(reference, actual) =>
        Validation.Error.Collection.Exactly(reference, actual)
    }
  }

  object collection extends collection

  trait date {
    def after(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.Date.After(equal, reference)

    def before(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.Date.Before(equal, reference)
  }

  object date extends date

  trait number {
    def equal[I: Numeric](expected: I, delta: I): Validation[I, Unit] = Validation.Number(
      Validation.Number.Operator.Equal,
      expected,
      delta
    )

    def equal[I](expected: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      equal(expected, delta = numeric.zero)

    def greaterThan[I](reference: I, equal: Boolean = false)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      Validation.Number(
        Validation.Number.Operator.GreaterThan(equal),
        reference,
        numeric.zero
      )

    def lessThan[I](reference: I, equal: Boolean = false)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      Validation.Number(
        Validation.Number.Operator.LessThan(equal),
        reference,
        numeric.zero
      )
  }

  object number extends number

  trait parsing {
    val bigDecimal: Validation[String, BigDecimal] = Validation.Parsing.BigDecimal

    val bigInt: Validation[String, BigInt] = Validation.Parsing.BigInt

    val double: Validation[String, Double] = Validation.Parsing.Double

    val float: Validation[String, Float] = Validation.Parsing.Float

    val int: Validation[String, Int] = Validation.Parsing.Int

    val long: Validation[String, Long] = Validation.Parsing.Long

    val short: Validation[String, Short] = Validation.Parsing.Short
  }

  object parsing extends parsing

  trait text {
    val length: Validation[String, Int] = lift(_.length)

    val trim: Validation[String, String] = lift(_.trim)

    def atLeast(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      Validation.Text.AtLeast(equal, reference)

    def atMost(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      Validation.Text.AtMost(equal, reference)

    val email: Validation[String, Unit] = matches("""^.+@.+\..+$""".r).modifyError {
      case Validation.Error.Text.Matches(_, actual) => Validation.Error.Text.Email(actual)
    }

    val empty: Validation[String, Unit] = atMost(reference = 0)

    def equal(expected: String): Validation[String, Unit] = Validation.Text.Equal(expected)

    val nonEmpty: Validation[String, Unit] = Validation.Not(empty)

    def exactly(expected: Int): Validation[String, Unit] = (atLeast(expected) and atMost(expected)).modifyError {
      case Validation.Error.Text.AtLeast(true, reference, actual) => Validation.Error.Text.Exactly(reference, actual)
      case Validation.Error.Text.AtMost(true, reference, actual)  => Validation.Error.Text.Exactly(reference, actual)
    }

    def matches(regex: Regex): Validation[String, Unit] = Validation.Text.Matches(regex)

    val required: Validation[String, String] = trim.andThen(nonEmpty.tap)
  }

  object text extends text
}
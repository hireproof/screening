package io.hireproof.screening

import cats.{Eq, Show, Traverse, UnorderedFoldable}

import java.time._
import java.util.UUID
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

object validations {
  def lift[I, O](f: I => O): Validation[I, O] = Validation.Lift(f)

  object collection {
    def atLeast[F[_]: UnorderedFoldable, A](reference: Long, equal: Boolean = true): Validation[F[A], Unit] =
      Validation.Collection.AtLeast(equal, reference)

    def atMost[F[_]: UnorderedFoldable, A](reference: Long, equal: Boolean = true): Validation[F[A], Unit] =
      Validation.Collection.AtMost(equal, reference)

    def contains[F[_]: Traverse, A: Eq: Show](reference: A): Validation[F[A], Unit] =
      Validation.Collection.Contains(reference)

    def empty[F[_]: UnorderedFoldable, A]: Validation[F[A], Unit] = atMost(reference = 0)

    def nonEmpty[F[_]: UnorderedFoldable, A]: Validation[F[A], Unit] = Validation.Not(empty)

    def exactly[F[_]: UnorderedFoldable, A](expected: Long): Validation[F[A], Unit] =
      (atLeast(expected, equal = true) and atMost(expected, equal = true)).modifyError {
        case Validation.Error.Collection.AtLeast(_, reference, actual) =>
          Validation.Error.Collection.Exactly(reference, actual)
        case Validation.Error.Collection.AtMost(_, reference, actual) =>
          Validation.Error.Collection.Exactly(reference, actual)
      }
  }

  object date {
    def after(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.Date.After(equal, reference)

    def before(reference: Instant, equal: Boolean = true): Validation[Instant, Unit] =
      Validation.Date.Before(equal, reference)
  }

  object duration {
    def atLeast(reference: FiniteDuration, equal: Boolean = true): Validation[FiniteDuration, Unit] =
      Validation.Duration.AtLeast(equal, reference)

    def atMost(reference: FiniteDuration, equal: Boolean = true): Validation[FiniteDuration, Unit] =
      Validation.Duration.AtMost(equal, reference)

    def exactly(reference: FiniteDuration): Validation[FiniteDuration, Unit] =
      (atLeast(reference, equal = true) and atMost(reference, equal = true)).modifyError {
        case Validation.Error.Duration.AtLeast(_, reference, actual) =>
          Validation.Error.Duration.Exactly(reference, actual)
        case Validation.Error.Duration.AtMost(_, reference, actual) =>
          Validation.Error.Duration.Exactly(reference, actual)
      }
  }

  object mapping {
    def apply[I: Show, O](
        f: I => Option[O],
        references: Option[Set[I]] = None
    ): Validation[I, O] = Validation.Mapping(f, references)

    def partial[I: Show, O](
        pf: PartialFunction[I, O],
        references: Option[Set[I]] = None
    ): Validation[I, O] = Validation.Mapping(pf.lift, references)
  }

  object number {
    def equal[I: Numeric](expected: I, delta: I): Validation[I, Unit] =
      Validation.Number(Validation.Number.Operator.Equal, expected, delta)

    def equal[I](expected: I)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      equal(expected, delta = numeric.zero)

    def greaterThan[I](reference: I, equal: Boolean = false)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      Validation.Number(Validation.Number.Operator.GreaterThan(equal), reference, numeric.zero)

    def greaterThanEqual[I: Numeric](reference: I): Validation[I, Unit] = greaterThan(reference, equal = true)

    def lessThan[I](reference: I, equal: Boolean = false)(implicit numeric: Numeric[I]): Validation[I, Unit] =
      Validation.Number(Validation.Number.Operator.LessThan(equal), reference, numeric.zero)

    def lessThanEqual[I: Numeric](reference: I): Validation[I, Unit] = lessThan(reference, equal = true)
  }

  object parsing {
    def apply[O](name: String)(parse: String => Option[O]): Validation[String, O] = Validation.Parsing(name, parse)
    val bigDecimal: Validation[String, BigDecimal] = parsing("BigDecimal")(parseBigDecimal)
    val bigInt: Validation[String, BigInt] = parsing("BigInt")(parseBigInt)
    val boolean: Validation[String, Boolean] = parsing("Boolean")(_.toBooleanOption)
    val double: Validation[String, Double] = parsing("Double")(_.toDoubleOption)
    val float: Validation[String, Float] = parsing("Float")(_.toFloatOption)
    val int: Validation[String, Int] = parsing("Int")(_.toIntOption)
    val long: Validation[String, Long] = parsing("Long")(_.toLongOption)
    val short: Validation[String, Short] = parsing("Short")(_.toShortOption)
    val uuid: Validation[String, UUID] = parsing("UUID")(parseUuid)
  }

  object text {
    val length: Validation[String, Int] = lift(_.length)

    val trim: Validation[String, String] = lift(_.trim)

    def atLeast(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      Validation.Text.AtLeast(equal, reference)

    def atMost(reference: Int, equal: Boolean = true): Validation[String, Unit] =
      Validation.Text.AtMost(equal, reference)

    val email: Validation[String, Unit] = matches("""^.+@.+$""".r).modifyError {
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
}

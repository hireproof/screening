package io.taig.inspector

import cats.data.Validated
import cats.syntax.all._

import java.time._
import java.time.temporal.Temporal
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._
import scala.util.matching.Regex

abstract class Validations[E] { self =>
  def errors: Validations.Errors[E]

  object collection {
    final def errors: Validations.Errors.Collection[E] = self.errors.collection

    val size: ValidationNel[Iterable[_], Nothing, Int] = Validation.transform(_.size)

    private def atLeastN(expected: Int): ValidationNel[Int, E, Unit] =
      numeric
        .greaterThanEquals(expected)
        .tapErrorWithNel((size, _) => errors.atLeast(expected, size))
        .withAttribute(Validation.Attribute.Collection.AtLeast(expected))

    def atLeast(expected: Int): ValidationNel[Iterable[_], E, Unit] = size.andThen(atLeastN(expected))

    private def atMostN(expected: Int): ValidationNel[Int, E, Unit] =
      numeric
        .lessThanEquals(expected)
        .tapErrorWithNel((size, _) => errors.atMost(expected, size))
        .withAttribute(Validation.Attribute.Collection.AtMost(expected))

    def atMost(expected: Int): ValidationNel[Iterable[_], E, Unit] = size.andThen(atMostN(expected))

    val empty: ValidationNel[Iterable[_], E, Unit] = atMost(expected = 0)

    val nonEmpty: ValidationNel[Iterable[_], E, Unit] = atLeast(expected = 1)

    def exactly(expected: Int): ValidationNel[Iterable[_], E, Unit] =
      size andThen atLeastN(expected)
        .and(atMostN(expected))
        .tapErrorWithNel((size, _) => errors.exactly(expected, size))

    def contains(reference: String): ValidationNel[Seq[String], E, Unit] = ValidationNel.check(
      input => Option.when(!input.contains(reference))(errors.contains(reference, input)),
      Validation.Attribute.Collection.Contains(reference)
    )
  }

  object date {
    final def errors: Validations.Errors.Date[E] = self.errors.date

    private def afterA[A <: Temporal](reference: A)(isAfter: (A, A) => Boolean): ValidationNel[A, E, Unit] =
      ValidationNel.check(
        input => Option.when(isAfter(reference, input))(errors.after(reference, input)),
        Validation.Attribute.Date.After(reference)
      )

    private def beforeA[A <: Temporal](reference: A)(isBefore: (A, A) => Boolean): ValidationNel[A, E, Unit] =
      ValidationNel.check(
        input => Option.when(isBefore(reference, input))(errors.before(reference, input)),
        Validation.Attribute.Date.Before(reference)
      )

    def after(reference: Instant): ValidationNel[Instant, E, Unit] = ValidationNel.check(
      input => Option.when(reference.isAfter(input))(errors.after(reference, input)),
      Validation.Attribute.Date.After(reference)
    )

    def before(reference: Instant): ValidationNel[Instant, E, Unit] = ValidationNel.check(
      input => Option.when(reference.isBefore(input))(errors.before(reference, input)),
      Validation.Attribute.Date.Before(reference)
    )

    def after(reference: OffsetDateTime): ValidationNel[OffsetDateTime, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: OffsetDateTime): ValidationNel[OffsetDateTime, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: LocalDateTime): ValidationNel[LocalDateTime, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: LocalDateTime): ValidationNel[LocalDateTime, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: OffsetTime): ValidationNel[OffsetTime, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: OffsetTime): ValidationNel[OffsetTime, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: LocalTime): ValidationNel[LocalTime, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: LocalTime): ValidationNel[LocalTime, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: Year): ValidationNel[Year, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: Year): ValidationNel[Year, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: YearMonth): ValidationNel[YearMonth, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: YearMonth): ValidationNel[YearMonth, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: LocalDate): ValidationNel[LocalDate, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: LocalDate): ValidationNel[LocalDate, E, Unit] = beforeA(reference)(_ isBefore _)

    def after(reference: ZonedDateTime): ValidationNel[ZonedDateTime, E, Unit] = afterA(reference)(_ isAfter _)

    def before(reference: ZonedDateTime): ValidationNel[ZonedDateTime, E, Unit] = beforeA(reference)(_ isBefore _)
  }

  object enumeration {
    final def errors: Validations.Errors.Enumeration[E] = self.errors.enumeration

    def oneOf(references: List[String]): ValidationNel[String, E, Unit] = ValidationNel.check(
      input => Option.when(!references.contains(input))(errors.oneOf(references, input)),
      Validation.Attribute.Enumeration.OneOf(references)
    )
  }

  object numeric {
    final def errors: Validations.Errors.Numeric[E] = self.errors.numeric

    def greaterThan[A: Numeric](expected: A): ValidationNel[A, E, Unit] = ValidationNel.check(
      input => Option.when(expected >= input)(errors.greaterThan(equals = false, expected.toDouble, input.toDouble)),
      Validation.Attribute.Numeric.GreaterThan(equals = false, expected.toDouble)
    )

    def greaterThanEquals[A: Numeric](expected: A): ValidationNel[A, E, Unit] = ValidationNel.check(
      input => Option.when(expected > input)(errors.greaterThan(equals = true, expected.toDouble, input.toDouble)),
      Validation.Attribute.Numeric.GreaterThan(equals = true, expected.toDouble)
    )

    def lessThan[A: Numeric](expected: A): ValidationNel[A, E, Unit] = ValidationNel.check(
      input => Option.when(expected <= input)(errors.lessThan(equals = false, expected.toDouble, input.toDouble)),
      Validation.Attribute.Numeric.LesserThan(equals = false, expected.toDouble)
    )

    def lessThanEquals[A: Numeric](expected: A): ValidationNel[A, E, Unit] = ValidationNel.check(
      input => Option.when(expected < input)(errors.lessThan(equals = true, expected.toDouble, input.toDouble)),
      Validation.Attribute.Numeric.LesserThan(equals = true, expected.toDouble)
    )

    def equals[A](expected: A, delta: A)(implicit numeric: Numeric[A]): ValidationNel[A, E, Unit] = {
      val value = expected.toDouble

      ValidationNel.check(
        { input =>
          val diff = (expected - input).abs
          Option.when(diff > delta)(errors.equal(value, input.toDouble))
        },
        Validation.Attribute.Numeric.GreaterThan(equals = true, value),
        Validation.Attribute.Numeric.LesserThan(equals = true, value)
      )
    }

    def equals[A](expected: A)(implicit numeric: Numeric[A]): ValidationNel[A, E, Unit] = equals(expected, numeric.zero)
  }

  object parsing {
    final def errors: Validations.Errors.Parsing[E] = self.errors.parsing

    private def apply[A](
        value: Validation.Attribute.Parsing.Value,
        f: String => Option[A]
    ): ValidationNel[String, E, A] =
      Validation.validate(
        input => Validated.fromOption(f(input), errors.apply(value, input)).toValidatedNel,
        Validation.Attribute.Parsing(value)
      )

    val bigDecimal: ValidationNel[String, E, BigDecimal] = apply(
      Validation.Attribute.Parsing.Value.BigDecimal,
      input => Either.catchOnly[NumberFormatException](BigDecimal(input)).toOption
    )

    val bigInt: ValidationNel[String, E, BigInt] = apply(
      Validation.Attribute.Parsing.Value.BigInt,
      input => Either.catchOnly[NumberFormatException](BigInt(input)).toOption
    )

    val double: ValidationNel[String, E, Double] = apply(Validation.Attribute.Parsing.Value.Double, _.toDoubleOption)

    val float: ValidationNel[String, E, Float] = apply(Validation.Attribute.Parsing.Value.Float, _.toFloatOption)

    val int: ValidationNel[String, E, Int] = apply(Validation.Attribute.Parsing.Value.Int, _.toIntOption)

    val long: ValidationNel[String, E, Long] = apply(Validation.Attribute.Parsing.Value.Long, _.toLongOption)

    val short: ValidationNel[String, E, Short] = apply(Validation.Attribute.Parsing.Value.Short, _.toShortOption)
  }

  object text {
    final def errors: Validations.Errors.Text[E] = self.errors.text

    val length: ValidationNel[String, Nothing, Int] = Validation.transform(_.length)

    val trim: ValidationNel[String, Nothing, String] = Validation.transform(_.trim)

    def matches(regex: Regex): ValidationNel[String, E, Unit] = ValidationNel.check(
      input => Option.when(!regex.matches(input))(errors.matches(regex, input)),
      Validation.Attribute.Text.Matches(regex)
    )

    val EmailRegex: Regex = """^.+@.+\..+$""".r

    val email: ValidationNel[String, E, Unit] = matches(EmailRegex)
      .tapErrorWithNel((value, _) => errors.email(value))

    private def atLeastN(reference: Int): ValidationNel[Int, E, Unit] =
      numeric
        .greaterThanEquals(reference)
        .tapErrorWithNel((size, _) => errors.atLeast(reference, size))
        .withAttribute(Validation.Attribute.Text.AtLeast(reference))

    def atLeast(reference: Int): ValidationNel[String, E, Unit] = length andThen atLeastN(reference)

    private def atMostN(reference: Int): ValidationNel[Int, E, Unit] =
      numeric
        .lessThanEquals(reference)
        .tapErrorWithNel((size, _) => errors.atMost(reference, size))
        .withAttribute(Validation.Attribute.Text.AtMost(reference))

    def atMost(reference: Int): ValidationNel[String, E, Unit] = length andThen atMostN(reference)

    val isEmpty: ValidationNel[String, E, Unit] = atMost(reference = 0)

    val nonEmpty: ValidationNel[String, E, Unit] = atLeast(reference = 1)

    def exactly(reference: Int): ValidationNel[String, E, Unit] = length andThen
      atLeastN(reference)
        .and(atMostN(reference))
        .tapErrorWithNel((size, _) => errors.exactly(reference, size))

    val required: ValidationNel[String, E, String] = trim andThen nonEmpty.tap
  }
}

object Validations {
  trait Errors[+E] { self =>
    def collection: Errors.Collection[E]

    def date: Errors.Date[E]

    def enumeration: Errors.Enumeration[E]

    def numeric: Errors.Numeric[E]

    def parsing: Errors.Parsing[E]

    def text: Errors.Text[E]

    final def map[T](f: E => T): Errors[T] = new Errors[T] {
      override def collection: Errors.Collection[T] = self.collection.map(f)

      override def date: Errors.Date[T] = self.date.map(f)

      override def enumeration: Errors.Enumeration[T] = self.enumeration.map(f)

      override def numeric: Errors.Numeric[T] = self.numeric.map(f)

      override def parsing: Errors.Parsing[T] = self.parsing.map(f)

      override def text: Errors.Text[T] = self.text.map(f)
    }
  }

  object Errors {
    trait Collection[+E] { self =>
      def atLeast(expected: Int, actual: Int): E

      def atMost(expected: Int, actual: Int): E

      def contains(reference: String, actual: Seq[String]): E

      def exactly(expected: Int, actual: Int): E

      final def map[T](f: E => T): Collection[T] = new Collection[T] {
        override def atLeast(expected: Int, actual: Int): T = f(self.atLeast(expected, actual))

        override def atMost(expected: Int, actual: Int): T = f(self.atMost(expected, actual))

        override def contains(reference: String, actual: Seq[String]): T = f(self.contains(reference, actual))

        override def exactly(expected: Int, actual: Int): T = f(self.exactly(expected, actual))
      }
    }

    trait Date[+E] { self =>
      def after(reference: Temporal, actual: Temporal): E

      def before(reference: Temporal, actual: Temporal): E

      final def map[T](f: E => T): Date[T] = new Date[T] {
        override def after(reference: Temporal, actual: Temporal): T = f(self.after(reference, actual))

        override def before(reference: Temporal, actual: Temporal): T = f(self.before(reference, actual))
      }
    }

    trait Enumeration[+E] { self =>
      def oneOf(references: List[String], actual: String): E

      final def map[T](f: E => T): Enumeration[T] = new Enumeration[T] {
        override def oneOf(references: List[String], actual: String): T = f(self.oneOf(references, actual))
      }
    }

    trait Numeric[+E] { self =>
      def equal(expected: Double, actual: Double): E

      def greaterThan(equals: Boolean, expected: Double, actual: Double): E

      def lessThan(equals: Boolean, expected: Double, actual: Double): E

      final def map[T](f: E => T): Numeric[T] = new Numeric[T] {
        override def equal(expected: Double, actual: Double): T = f(self.equal(expected, actual))

        override def greaterThan(equals: Boolean, expected: Double, actual: Double): T =
          f(self.greaterThan(equals, expected, actual))

        override def lessThan(equals: Boolean, expected: Double, actual: Double): T =
          f(self.lessThan(equals, expected, actual))
      }
    }

    trait Parsing[+E] { self =>
      def apply(value: Validation.Attribute.Parsing.Value, actual: String): E

      final def map[T](f: E => T): Parsing[T] = new Parsing[T] {
        override def apply(value: Validation.Attribute.Parsing.Value, actual: String): T = f(self.apply(value, actual))
      }
    }

    trait Text[+E] { self =>
      def atLeast(reference: Int, actual: Int): E

      def atMost(reference: Int, actual: Int): E

      def email(actual: String): E

      def equal(expected: String, actual: String): E

      def exactly(expected: Int, actual: Int): E

      def matches(regex: Regex, actual: String): E

      final def map[T](f: E => T): Text[T] = new Text[T] {
        override def atLeast(reference: Int, actual: Int): T = f(self.atLeast(reference, actual))

        override def atMost(reference: Int, actual: Int): T = f(self.atMost(reference, actual))

        override def email(actual: String): T = f(self.email(actual))

        override def equal(expected: String, actual: String): T = f(self.equal(expected, actual))

        override def exactly(expected: Int, actual: Int): T = f(self.exactly(expected, actual))

        override def matches(regex: Regex, actual: String): T = f(self.matches(regex, actual))
      }
    }
  }
}

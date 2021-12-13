package io.taig.inspector

import cats.arrow.Arrow
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import cats.{Eq, Show}

import java.time.temporal.Temporal
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.util.matching.Regex

sealed abstract class Validation[-I, O] {
  def run(input: I): ValidatedNel[Validation.Error, O]

  def errors(input: I): List[Validation.Error]

  final def andThen[T](validation: Validation[O, T]): Validation[I, T] = Validation.AndThen(this, validation)

  final def modifyErrors(f: NonEmptyList[Validation.Error] => NonEmptyList[Validation.Error]): Validation[I, O] =
    Validation.Modify(this, f)

  final def modifyError(pf: PartialFunction[Validation.Error, Validation.Error]): Validation[I, O] =
    modifyErrors(_.map(error => pf.applyOrElse(error, (_: Validation.Error) => error)))

  final def withErrors(errors: NonEmptyList[Validation.Error]): Validation[I, O] = modifyErrors(_ => errors)

  final def withError(error: Validation.Error): Validation[I, O] = withErrors(NonEmptyList.one(error))
}

object Validation {
  sealed abstract class Collection[F[_], A] extends Validation[F[A], Unit]

  object Collection {
    final case class AtLeast(reference: Int) extends Collection[Iterable, Any] {
      def error(actual: Int): Validation.Error = Validation.Error.Collection.AtLeast(reference, actual)

      override def errors(input: Iterable[Any]): List[Validation.Error] = List(error(input.size))

      override def run(input: Iterable[Any]): Validated[NonEmptyList[Validation.Error], Unit] = {
        val size = input.size
        Validated.cond(size >= reference, (), NonEmptyList.one(error(size)))
      }
    }

    final case class AtMost(reference: Int) extends Collection[Iterable, Any] {
      def error(actual: Int): Validation.Error = Validation.Error.Collection.AtMost(reference, actual)

      override def errors(input: Iterable[Any]): List[Validation.Error] = List(error(input.size))

      override def run(input: Iterable[Any]): Validated[NonEmptyList[Validation.Error], Unit] = {
        val size = input.size
        Validated.cond(size <= reference, (), NonEmptyList.one(error(size)))
      }
    }

    final case class Contains[A: Eq: Show](reference: A) extends Collection[Seq, A] {
      def error(input: Seq[A]): Validation.Error =
        Validation.Error.Collection.Contains(reference.show, input.map(_.show))

      override def errors(input: Seq[A]): List[Validation.Error] = List(error(input))

      override def run(input: Seq[A]): Validated[NonEmptyList[Validation.Error], Unit] =
        Validated.cond(input.contains_(reference), (), NonEmptyList.one(error(input)))
    }
  }

  sealed abstract class Date[I <: Temporal](reference: I, check: (I, I) => Boolean) extends Validation[I, Unit] {
    def error(input: I): Validation.Error.Date

    final override def errors(input: I): List[Validation.Error.Date] = List(error(input))

    final override def run(input: I): Validated[NonEmptyList[Validation.Error], Unit] =
      Validated.condNel(check(input, reference), (), error(input))
  }

  object Date {
    final case class After[I <: Temporal](reference: I)(check: (I, I) => Boolean) extends Date[I](reference, check) {
      override def error(input: I): Error.Date = Validation.Error.Date.After(reference, input)
    }

    final case class Before[I <: Temporal](reference: I)(check: (I, I) => Boolean) extends Date[I](reference, check) {
      override def error(input: I): Error.Date = Validation.Error.Date.Before(reference, input)
    }
  }

  final case class Number[I: Numeric](operator: Number.Operator, reference: I, delta: I) extends Validation[I, Unit] {
    def error(input: I): Validation.Error = {
      val error: (Double, Double) => Validation.Error = operator match {
        case Number.Operator.Equal              => Validation.Error.Number.Equal.apply
        case Number.Operator.GreaterThan(equal) => Validation.Error.Number.GreaterThan(equal, _, _)
        case Number.Operator.LessThan(equal)    => Validation.Error.Number.LessThan(equal, _, _)
      }

      error(reference.toDouble, input.toDouble)
    }

    override def errors(input: I): List[Validation.Error] = List(error(input))

    override def run(input: I): ValidatedNel[Validation.Error, Unit] = {
      val valid = operator match {
        case Number.Operator.Equal              => (input - reference).abs <= delta
        case Number.Operator.GreaterThan(true)  => input >= reference
        case Number.Operator.GreaterThan(false) => input > reference
        case Number.Operator.LessThan(true)     => input <= reference
        case Number.Operator.LessThan(false)    => input < reference
      }

      Validated.cond(valid, (), NonEmptyList.one(error(input)))
    }
  }

  object Number {
    sealed abstract class Operator extends Product with Serializable

    object Operator {
      final case object Equal extends Operator
      final case class GreaterThan(equal: Boolean) extends Operator
      final case class LessThan(equal: Boolean) extends Operator
    }
  }

  sealed abstract class Parsing[O](value: Validation.Parsing.Value, parse: String => Option[O])
      extends Validation[String, O] {
    def error(input: String): Validation.Error.Parsing = Validation.Error.Parsing(value, input)

    final override def errors(input: String): List[Validation.Error] = List(error(input))

    final override def run(input: String): Validated[NonEmptyList[Validation.Error], O] =
      parse(input).toValidNel(error(input))
  }

  object Parsing {
    sealed abstract class Value extends Product with Serializable

    object Value {
      final case object BigDecimal extends Value
      final case object BigInt extends Value
      final case object Double extends Value
      final case object Float extends Value
      final case object Int extends Value
      final case object Long extends Value
      final case object Short extends Value
    }

    final case object BigDecimal extends Parsing(Validation.Parsing.Value.BigDecimal, parseBigDecimal)
    final case object BigInt extends Parsing(Validation.Parsing.Value.BigInt, parseBigInt)
    final case object Double extends Parsing(Validation.Parsing.Value.Double, _.toDoubleOption)
    final case object Float extends Parsing(Validation.Parsing.Value.Float, _.toFloatOption)
    final case object Int extends Parsing(Validation.Parsing.Value.Int, _.toIntOption)
    final case object Long extends Parsing(Validation.Parsing.Value.Long, _.toLongOption)
    final case object Short extends Parsing(Validation.Parsing.Value.Short, _.toShortOption)
  }

  sealed abstract class Text extends Validation[String, Unit]

  object Text {
    final case class AtLeast(equal: Boolean, reference: Int) extends Text {
      def error(length: Int): Validation.Error = Validation.Error.Text.AtLeast(equal, reference, length)

      override def errors(input: String): List[Validation.Error] = List(error(input.length))

      override def run(input: String): Validated[NonEmptyList[Validation.Error], Unit] = {
        val length = input.length
        val check = if (equal) length >= reference else length > reference
        Validated.cond(check, (), NonEmptyList.one(error(length)))
      }
    }

    final case class AtMost(equal: Boolean, reference: Int) extends Text {
      def error(length: Int): Validation.Error = Validation.Error.Text.AtMost(equal, reference, length)

      override def errors(input: String): List[Validation.Error] = List(error(input.length))

      override def run(input: String): Validated[NonEmptyList[Validation.Error], Unit] = {
        val length = input.length
        val check = if (equal) length <= reference else length < reference
        Validated.cond(check, (), NonEmptyList.one(error(length)))
      }
    }

    final case class Equal(expected: String) extends Text {
      def error(input: String): Validation.Error = Validation.Error.Text.Equal(expected, input)

      override def errors(input: String): List[Error] = List(error(input))

      override def run(input: String): Validated[NonEmptyList[Error], Unit] =
        Validated.cond(expected == input, (), NonEmptyList.one(error(input)))
    }

    final case class Matches(regex: Regex) extends Text {
      def error(input: String): Validation.Error = Validation.Error.Text.Matches(regex, input)

      override def errors(input: String): List[Validation.Error] = List(error(input))

      override def run(input: String): Validated[NonEmptyList[Validation.Error], Unit] =
        Validated.cond(regex.matches(input), (), NonEmptyList.one(error(input)))
    }
  }

  final case class AndThen[I, X, O](left: Validation[I, X], right: Validation[X, O]) extends Validation[I, O] {
    override def errors(input: I): List[Validation.Error] = left.run(input) match {
      case Validated.Valid(x)        => left.errors(input) ++ right.errors(x)
      case Validated.Invalid(errors) => errors.toList
    }

    override def run(input: I): ValidatedNel[Validation.Error, O] = left.run(input).andThen(right.run)
  }

  final case class And[I](left: Validation[I, Unit], right: Validation[I, Unit]) extends Validation[I, Unit] {
    override def errors(input: I): List[Validation.Error] = left.errors(input) ++ right.errors(input)

    override def run(input: I): ValidatedNel[Validation.Error, Unit] = (left.run(input), right.run(input)) match {
      case (Validated.Invalid(left), Validated.Invalid(right)) => Validated.invalid(left concatNel right)
      case (left @ Validated.Invalid(_), _)                    => left
      case (_, right @ Validated.Invalid(_))                   => right
      case (left @ Validated.Valid(_), Validated.Valid(_))     => left
    }
  }

  final case class Or[I](left: Validation[I, Unit], right: Validation[I, Unit]) extends Validation[I, Unit] {
    override def errors(input: I): List[Validation.Error] = left.errors(input) ++ right.errors(input)

    override def run(input: I): ValidatedNel[Validation.Error, Unit] = (left.run(input), right.run(input)) match {
      case (left @ Validated.Valid(_), _)                      => left
      case (_, right @ Validated.Valid(_))                     => right
      case (Validated.Invalid(left), Validated.Invalid(right)) => Validated.invalid(left concatNel right)
    }
  }

  final case class Not[I](validation: Validation[I, Unit]) extends Validation[I, Unit] {
    override def errors(input: I): List[Validation.Error] = validation.errors(input).map(Validation.Error.Not.apply)

    override def run(input: I): ValidatedNel[Validation.Error, Unit] = validation.run(input) match {
      case Validated.Valid(_) =>
        NonEmptyList
          .fromList(validation.errors(input))
          .fold[ValidatedNel[Validation.Error, Unit]](Validated.valid(()))(Validated.invalid)
          .leftMap(_.map(Validation.Error.Not.apply))
      case Validated.Invalid(_) => Validated.valid(())
    }
  }

  final case class Lift[I, O](f: I => O) extends Validation[I, O] {
    override def errors(input: I): List[Validation.Error] = Nil

    override def run(input: I): ValidatedNel[Validation.Error, O] = Validated.valid(f(input))
  }

  final case class First[I, X, O](validation: Validation[I, O]) extends Validation[(I, X), (O, X)] {
    override def errors(input: (I, X)): List[Validation.Error] = validation.errors(input._1)

    override def run(input: (I, X)): ValidatedNel[Validation.Error, (O, X)] =
      validation.run(input._1).map((_, input._2))
  }

  final case class Modify[I, O](
      validation: Validation[I, O],
      f: NonEmptyList[Validation.Error] => NonEmptyList[Validation.Error]
  ) extends Validation[I, O] {
    override def errors(input: I): List[Validation.Error] = validation.errors(input).toNel.map(f(_).toList).orEmpty

    override def run(input: I): Validated[NonEmptyList[Validation.Error], O] = validation.run(input).leftMap(f)
  }

  implicit final class Ops[I, O](val validation: Validation[I, O]) extends AnyVal {
    def tap: Validation[I, I] = validation.first[I].dimap((i: I) => (i, i))(_._2)
  }

  implicit final class UnitOps[I](val validation: Validation[I, Unit]) extends AnyVal {
    def and(validation: Validation[I, Unit]): Validation[I, Unit] = Validation.And(this.validation, validation)

    def or(validation: Validation[I, Unit]): Validation[I, Unit] = Validation.Or(this.validation, validation)
  }

  sealed abstract class Error extends Product with Serializable

  object Error {
    final case class Not(error: Error) extends Error

    object Not {
      def apply(error: Error): Error = error match {
        case Not(error)                                   => error
        case Collection.AtLeast(reference, actual)        => Collection.AtMost(reference, actual)
        case Collection.AtMost(reference, actual)         => Collection.AtLeast(reference, actual)
        case Date.After(reference, actual)                => Date.Before(reference, actual)
        case Date.Before(reference, actual)               => Date.After(reference, actual)
        case Number.GreaterThan(equal, reference, actual) => Number.LessThan(!equal, reference, actual)
        case Number.LessThan(equal, reference, actual)    => Number.GreaterThan(!equal, reference, actual)
        case Text.AtLeast(equal, reference, actual)       => Text.AtMost(!equal, reference, actual)
        case Text.AtMost(equal, reference, actual)        => Text.AtLeast(!equal, reference, actual)
        case error                                        => new Not(error)
      }
    }

    sealed abstract class Collection extends Error

    object Collection {
      final case class AtLeast(reference: Int, actual: Int) extends Collection
      final case class AtMost(reference: Int, actual: Int) extends Collection
      final case class Contains(reference: String, actual: Seq[String]) extends Collection
      final case class Exactly(expected: Int, actual: Int) extends Collection
    }

    sealed abstract class Date extends Error

    object Date {
      final case class After(reference: Temporal, actual: Temporal) extends Date
      final case class Before(reference: Temporal, actual: Temporal) extends Date
    }

    sealed abstract class Number extends Error

    object Number {
      final case class Equal(expected: Double, actual: Double) extends Number
      final case class GreaterThan(equal: Boolean, reference: Double, actual: Double) extends Number
      final case class LessThan(equal: Boolean, expected: Double, actual: Double) extends Number
    }

    final case class Parsing(expected: Validation.Parsing.Value, actual: String) extends Error

    sealed abstract class Text extends Error

    object Text {
      final case class AtLeast(equal: Boolean, reference: Int, actual: Int) extends Text
      final case class AtMost(equal: Boolean, reference: Int, actual: Int) extends Text
      final case class Email(actual: String) extends Text
      final case class Equal(expected: String, actual: String) extends Text
      final case class Exactly(expected: Int, actual: Int) extends Text
      final case class Matches(regex: Regex, actual: String) extends Text
    }
  }

  implicit val arrow: Arrow[Validation] = new Arrow[Validation] {
    override def lift[A, B](f: A => B): Validation[A, B] = Lift(f)

    override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = First(fa)

    override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] = AndThen(g, f)
  }
}

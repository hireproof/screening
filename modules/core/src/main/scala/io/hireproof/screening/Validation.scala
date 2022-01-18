package io.hireproof.screening

import cats.arrow.Arrow
import cats.data.{NonEmptyList, NonEmptyMap, Validated, ValidatedNel}
import cats.syntax.all._
import cats.{Eq, Semigroup, Show}

import java.time.Instant
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

sealed abstract class Validation[-I, +O] {
  def run(input: I): ValidatedNel[Validation.Error, O]

  def errors(input: I): List[Validation.Error]

  final def map[T](f: O => T): Validation[I, T] = Validation.arrow.rmap(this)(f)

  final def andThen[T](validation: Validation[O, T]): Validation[I, T] = Validation.AndThen(this, validation)

  final def collect[T](f: PartialFunction[O, T]): Validation[I, T] = map(f.lift).required

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
    final case class AtLeast[F[X] <: Iterable[X], A](equal: Boolean, reference: Int) extends Collection[F, A] {
      def error(actual: Int): Validation.Error = Validation.Error.Collection.AtLeast(equal, reference, actual)

      override def errors(input: F[A]): List[Error] = List(error(input.size))

      override def run(input: F[A]): ValidatedNel[Error, Unit] = {
        val size = input.size
        val check = if (equal) size >= reference else size > reference
        Validated.cond(check, (), NonEmptyList.one(error(size)))
      }
    }

    final case class AtMost[F[X] <: Iterable[X], A](equal: Boolean, reference: Int) extends Collection[F, A] {
      def error(actual: Int): Validation.Error = Validation.Error.Collection.AtMost(equal, reference, actual)

      override def errors(input: F[A]): List[Error] = List(error(input.size))

      override def run(input: F[A]): ValidatedNel[Error, Unit] = {
        val size = input.size
        val check = if (equal) size <= reference else size < reference
        Validated.cond(check, (), NonEmptyList.one(error(size)))
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

  sealed abstract class Date extends Validation[Instant, Unit]

  object Date {
    final case class After(equal: Boolean, reference: Instant) extends Date {
      def error(input: Instant): Error.Date = Validation.Error.Date.After(equal, reference, input)

      override def errors(input: Instant): List[Error] = List(error(input))

      override def run(input: Instant): ValidatedNel[Error, Unit] = {
        val compare = reference.compareTo(input)
        val check = if (equal) compare <= 0 else compare < 0
        Validated.condNel(check, (), error(input))
      }
    }

    final case class Before(equal: Boolean, reference: Instant) extends Date {
      def error(input: Instant): Error.Date = Validation.Error.Date.Before(equal, reference, input)

      override def errors(input: Instant): List[Error] = List(error(input))

      override def run(input: Instant): ValidatedNel[Error, Unit] = {
        val compare = reference.compareTo(input)
        val check = if (equal) compare >= 0 else compare > 0
        Validated.condNel(check, (), error(input))
      }
    }
  }

  sealed abstract class Duration extends Validation[FiniteDuration, Unit]

  object Duration {
    final case class AtLeast(equal: Boolean, reference: FiniteDuration) extends Duration {
      def error(input: FiniteDuration): Validation.Error = Validation.Error.Duration.AtLeast(equal, reference, input)

      override def errors(input: FiniteDuration): List[Error] = List(error(input))

      override def run(input: FiniteDuration): ValidatedNel[Error, Unit] = {
        val compare = reference.compareTo(input)
        val check = if (equal) compare <= 0 else compare < 0
        Validated.condNel(check, (), error(input))
      }
    }

    final case class AtMost(equal: Boolean, reference: FiniteDuration) extends Duration {
      def error(input: FiniteDuration): Validation.Error = Validation.Error.Duration.AtMost(equal, reference, input)

      override def errors(input: FiniteDuration): List[Error] = List(error(input))

      override def run(input: FiniteDuration): ValidatedNel[Error, Unit] = {
        val compare = reference.compareTo(input)
        val check = if (equal) compare >= 0 else compare > 0
        Validated.condNel(check, (), error(input))
      }
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
      case object Equal extends Operator
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
      case object BigDecimal extends Value
      case object BigInt extends Value
      case object Instant extends Value
      case object Double extends Value
      case object Float extends Value
      case object Int extends Value
      case object Long extends Value
      case object Short extends Value
      case object Uuid extends Value
    }

    case object BigDecimal extends Parsing(Validation.Parsing.Value.BigDecimal, parseBigDecimal)
    case object BigInt extends Parsing(Validation.Parsing.Value.BigInt, parseBigInt)
    case object Double extends Parsing(Validation.Parsing.Value.Double, _.toDoubleOption)
    case object Float extends Parsing(Validation.Parsing.Value.Float, _.toFloatOption)
    case object Instant extends Parsing(Validation.Parsing.Value.Instant, parseInstant)
    case object Int extends Parsing(Validation.Parsing.Value.Int, _.toIntOption)
    case object Long extends Parsing(Validation.Parsing.Value.Long, _.toLongOption)
    case object Short extends Parsing(Validation.Parsing.Value.Short, _.toShortOption)
    case object Uuid extends Parsing(Validation.Parsing.Value.Uuid, parseUuid)
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

    final case class Equal(reference: String) extends Text {
      def error(input: String): Validation.Error = Validation.Error.Text.Equal(reference, input)

      override def errors(input: String): List[Error] = List(error(input))

      override def run(input: String): Validated[NonEmptyList[Error], Unit] =
        Validated.cond(reference == input, (), NonEmptyList.one(error(input)))
    }

    final case class Matches(regex: Regex) extends Text {
      def error(input: String): Validation.Error = Validation.Error.Text.Matches(regex, input)

      override def errors(input: String): List[Validation.Error] = List(error(input))

      override def run(input: String): Validated[NonEmptyList[Validation.Error], Unit] =
        Validated.cond(regex.matches(input), (), NonEmptyList.one(error(input)))
    }
  }

  final case class Mapping[I, O](f: I => Option[O], references: Option[Set[I]], render: I => String)
      extends Validation[I, O] {
    override def run(input: I): ValidatedNel[Error, O] = Validated.fromOption(f(input), NonEmptyList.one(error(input)))

    override def errors(input: I): List[Error] = List(error(input))

    def error(input: I): Error = Error.Mapping(references.map(_.map(render)), render(input))
  }

  object Optional {
    final case class Required[I, O](validation: Validation[I, Option[O]]) extends Validation[I, O] {
      override def run(input: I): ValidatedNel[Error, O] = validation.run(input).andThen {
        case Some(value) => Validated.valid(value)
        case None        => Validated.invalidNel(Error.Optional.Required)
      }

      override def errors(input: I): List[Error] = List(Error.Optional.Required)
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

  final case class Or[I, O](left: Validation[I, O], right: Validation[I, O]) extends Validation[I, O] {
    override def errors(input: I): List[Validation.Error] = left.errors(input) ++ right.errors(input)

    override def run(input: I): ValidatedNel[Validation.Error, O] = (left.run(input), right.run(input)) match {
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

  final case class Invalid(errors: NonEmptyList[Validation.Error]) extends Validation[Any, Nothing] {
    override def run(input: Any): ValidatedNel[Error, Nothing] = Validated.invalid(errors)

    override def errors(input: Any): List[Error] = errors.toList
  }

  final case class Modify[I, O](
      validation: Validation[I, O],
      f: NonEmptyList[Validation.Error] => NonEmptyList[Validation.Error]
  ) extends Validation[I, O] {
    override def errors(input: I): List[Validation.Error] = validation.errors(input).toNel.map(f(_).toList).orEmpty

    override def run(input: I): Validated[NonEmptyList[Validation.Error], O] = validation.run(input).leftMap(f)
  }

  implicit final class Ops[I, O](val validation: Validation[I, O]) extends AnyVal {
    def required[T](implicit ev: O =:= Option[T]): Validation[I, T] =
      Validation.Optional.Required(validation.map(ev.apply))

    def tap: Validation[I, I] = validation.first[I].dimap((i: I) => (i, i))(_._2)

    def or(right: Validation[I, O]): Validation[I, O] = Validation.Or(validation, right)
  }

  implicit final class UnitOps[I](val validation: Validation[I, Unit]) extends AnyVal {
    def and(right: Validation[I, Unit]): Validation[I, Unit] = Validation.And(validation, right)
  }

  sealed abstract class Error extends Product with Serializable

  object Error {
    final case class Not(error: Error) extends Error

    object Not {
      def apply(error: Error): Error = error match {
        case Not(error)                                   => error
        case Collection.AtLeast(equal, reference, actual) => Collection.AtMost(!equal, reference, actual)
        case Collection.AtMost(equal, reference, actual)  => Collection.AtLeast(!equal, reference, actual)
        case Date.After(equal, reference, actual)         => Date.Before(!equal, reference, actual)
        case Date.Before(equal, reference, actual)        => Date.After(!equal, reference, actual)
        case Duration.AtLeast(equal, reference, actual)   => Duration.AtMost(!equal, reference, actual)
        case Duration.AtMost(equal, reference, actual)    => Duration.AtLeast(!equal, reference, actual)
        case Number.GreaterThan(equal, reference, actual) => Number.LessThan(!equal, reference, actual)
        case Number.LessThan(equal, reference, actual)    => Number.GreaterThan(!equal, reference, actual)
        case Text.AtLeast(equal, reference, actual)       => Text.AtMost(!equal, reference, actual)
        case Text.AtMost(equal, reference, actual)        => Text.AtLeast(!equal, reference, actual)
        case error                                        => new Not(error)
      }
    }

    sealed abstract class Collection extends Error

    object Collection {
      final case class AtLeast(equal: Boolean, reference: Int, actual: Int) extends Collection
      final case class AtMost(equal: Boolean, reference: Int, actual: Int) extends Collection
      final case class Contains(reference: String, actual: Seq[String]) extends Collection
      final case class Exactly(reference: Int, actual: Int) extends Collection
    }

    final case class Conflict(actual: String) extends Error

    sealed abstract class Date extends Error

    object Date {
      final case class After(equal: Boolean, reference: Instant, actual: Instant) extends Date
      final case class Before(equal: Boolean, reference: Instant, actual: Instant) extends Date
    }

    sealed abstract class Duration extends Error

    object Duration {
      final case class AtLeast(equal: Boolean, reference: FiniteDuration, actual: FiniteDuration) extends Duration
      final case class AtMost(equal: Boolean, reference: FiniteDuration, actual: FiniteDuration) extends Duration
      final case class Exactly(reference: FiniteDuration, actual: FiniteDuration) extends Duration
    }

    final case class Mapping(references: Option[Set[String]], actual: String) extends Error

    sealed abstract class Optional extends Error

    object Optional {
      case object Required extends Optional
    }

    sealed abstract class Number extends Error

    object Number {
      final case class Equal(reference: Double, actual: Double) extends Number
      final case class GreaterThan(equal: Boolean, reference: Double, actual: Double) extends Number
      final case class LessThan(equal: Boolean, reference: Double, actual: Double) extends Number
    }

    final case class Parsing(reference: Validation.Parsing.Value, actual: String) extends Error

    sealed abstract class Text extends Error

    object Text {
      final case class AtLeast(equal: Boolean, reference: Int, actual: Int) extends Text
      final case class AtMost(equal: Boolean, reference: Int, actual: Int) extends Text
      final case class Email(actual: String) extends Text
      final case class Equal(reference: String, actual: String) extends Text
      final case class Exactly(reference: Int, actual: Int) extends Text
      final case class Matches(regex: Regex, actual: String) extends Text
    }

    final case class Unknown(actual: String) extends Error

    implicit val eq: Eq[Validation.Error] = Eq.fromUniversalEquals
  }

  final case class Errors(toNem: NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]]) {
    def modifyHistory(f: Selection.History => Selection.History): Validation.Errors = Errors(toNem.mapKeys(f))

    def modifyError(f: NonEmptyList[Validation.Error] => NonEmptyList[Validation.Error]): Validation.Errors = Errors(
      toNem.map(f)
    )

    def modifyErrors(f: Validation.Error => Validation.Error): Validation.Errors = modifyError(_.map(f))

    def merge(errors: Validation.Errors): Validation.Errors = this |+| errors

    def get(history: Selection.History): List[Validation.Error] = toNem(history).map(_.toList).orEmpty
  }

  object Errors {
    def ofErrors(
        head: (Selection.History, NonEmptyList[Validation.Error]),
        tail: (Selection.History, NonEmptyList[Validation.Error])*
    ): Validation.Errors = Errors(NonEmptyMap.of(head, tail: _*))

    def ofError(
        head: (Selection.History, Validation.Error),
        tail: (Selection.History, Validation.Error)*
    ): Validation.Errors =
      Errors(NonEmptyMap.of(head.map(NonEmptyList.one), tail.map(_.map(NonEmptyList.one)): _*))

    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Validation.Errors =
      Errors(NonEmptyMap.one(history, errors))

    def oneNel(history: Selection.History, error: Validation.Error): Validation.Errors =
      one(history, NonEmptyList.one(error))

    def root(errors: NonEmptyList[Validation.Error]): Validation.Errors = one(Selection.History.Root, errors)

    def rootNel(error: Validation.Error): Validation.Errors = oneNel(Selection.History.Root, error)

    def fromMap(values: SortedMap[Selection.History, NonEmptyList[Validation.Error]]): Option[Validation.Errors] =
      NonEmptyMap.fromMap(values).map(Errors.apply)

    implicit val semigroup: Semigroup[Validation.Errors] = new Semigroup[Validation.Errors] {
      override def combine(x: Errors, y: Errors): Errors = Errors(x.toNem |+| y.toNem)
    }
  }

  def ask[A]: Validation[A, A] = Lift(identity)

  def valid[A](value: A): Validation[Any, A] = Lift(_ => value)

  def invalid[A](errors: NonEmptyList[Validation.Error]): Validation[Any, Nothing] = Invalid(errors)

  def invalidNel[A](error: Validation.Error): Validation[Any, Nothing] = Invalid(NonEmptyList.one(error))

  implicit val arrow: Arrow[Validation] = new Arrow[Validation] {
    override def lift[A, B](f: A => B): Validation[A, B] = Lift(f)

    override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = First(fa)

    override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] = AndThen(g, f)
  }
}

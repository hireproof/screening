package io.taig.inspector

import cats.arrow.Arrow
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import cats.{CommutativeApplicative, Semigroup, SemigroupK, Traverse}

import java.time.temporal.Temporal
import scala.util.matching.Regex

abstract class Validation[-I, +E, O] {
  def attributes: List[Validation.Attribute]

  def run(input: I): Validated[E, O]

  final def modifyAttributes(f: List[Validation.Attribute] => List[Validation.Attribute]): Validation[I, E, O] =
    Validation(run, f(attributes))

  final def withAttributes(attributes: List[Validation.Attribute]): Validation[I, E, O] =
    modifyAttributes(_ => attributes)

  final def withAttribute(attribute: Validation.Attribute): Validation[I, E, O] = withAttributes(List(attribute))

  final def map[T](f: O => T): Validation[I, E, T] = mapResult(_.map(f))

  final def as[T](value: => T): Validation[I, E, T] = map(_ => value)

  final def void: Validation[I, E, Unit] = as(())

  final def mapError[T](f: E => T): Validation[I, T, O] = mapResult(_.leftMap(f))

  final def asError[T](error: T): Validation[I, T, O] = mapError(_ => error)

//  final def modifyError[E1 >: E](pf: PartialFunction[E, E1]): Validation[I, E1, O] =
//    mapError(error => pf.applyOrElse(error, (_: E) => error))

  final def mapResult[T, U](f: Validated[E, O] => Validated[T, U]): Validation[I, T, U] =
    Validation(input => f(run(input)), attributes)

  final def local[T](f: T => I): Validation[T, E, O] = Validation(input => run(f(input)), attributes)

  /** Performs [[local]] and [[map]] simultaneously */
  def dimap[T, U](f: T => I)(g: O => U): Validation[T, E, U] = Validation(input => run(f(input)).map(g), attributes)

  /** Discard computed O and yield the input value */
  final def tap[I1 <: I]: Validation[I1, E, I1] = Validation(input => run(input).map(_ => input), attributes)

  /** Yield computed O combined with input value */
  final def tapWith[I1 <: I, T](f: (I1, O) => T): Validation[I1, E, T] =
    Validation(input => run(input).map(errors => f(input, errors)), attributes)

  final def tapErrorWith[I1 <: I, T](f: (I1, E) => T): Validation[I1, T, O] =
    Validation(input => run(input).leftMap(f(input, _)), attributes)

  final def tapErrorWithNel[I1 <: I, T](f: (I1, E) => T): ValidationNel[I1, T, O] =
    Validation(input => run(input).leftMap(error => NonEmptyList.one(f(input, error))), attributes)

  final def andThen[O1 >: O, E1 >: E, T](validation: Validation[O1, E1, T]): Validation[I, E1, T] =
    Validation(input => run(input).andThen(validation.run), attributes ++ validation.attributes)

  final def and[I1 <: I, E1 >: E: Semigroup](validation: Validation[I1, E1, Unit]): Validation[I1, E1, O] =
    Validation(input => validation.run(input) *> run(input), attributes ++ validation.attributes)
}

object Validation extends ValidationInstances1 {
  sealed abstract class Attribute extends Product with Serializable

  object Attribute {
    sealed abstract class Collection extends Attribute

    object Collection {
      final case class AtLeast(expected: Int) extends Collection
      final case class AtMost(expected: Int) extends Collection
      final case class Contains(reference: String) extends Collection
    }

    sealed abstract class Date extends Attribute

    object Date {
      final case class After(reference: Temporal) extends Date
      final case class Before(reference: Temporal) extends Date
    }

    sealed abstract class Enumeration extends Attribute

    object Enumeration {
      final case class OneOf(references: List[String]) extends Enumeration
    }

    sealed abstract class Numeric extends Attribute

    object Numeric {
      final case class GreaterThan(equals: Boolean, expected: Double) extends Numeric
      final case class LesserThan(equals: Boolean, expected: Double) extends Numeric
    }

    final case class Parsing(value: Parsing.Value) extends Attribute

    object Parsing {
      sealed abstract class Value extends Product with Serializable

      object Value {
        case object BigDecimal extends Value
        case object BigInt extends Value
        case object Double extends Value
        case object Float extends Value
        case object Int extends Value
        case object Long extends Value
        case object Short extends Value
      }
    }

    sealed abstract class Text extends Attribute

    object Text {
      final case class AtLeast(reference: Int) extends Text
      final case class AtMost(reference: Int) extends Text
      final case class Matches(regex: Regex) extends Text
    }

    sealed abstract class Time extends Attribute

    object Time {
      final case class After(expected: Temporal) extends Time
      final case class Before(expected: Temporal) extends Time
    }
  }

  def apply[I, E, O](f: I => Validated[E, O], attributes: List[Validation.Attribute] = Nil): Validation[I, E, O] = {
    val _attributes = attributes

    new Validation[I, E, O] {
      override def attributes: List[Validation.Attribute] = _attributes

      override def run(input: I): Validated[E, O] = f(input)
    }
  }

  def valid[O](value: O): Validation[Any, Nothing, O] = Validation(_ => Validated.valid(value))

  val unit: Validation[Any, Nothing, Unit] = valid(())

  def invalid[E](error: E): Validation[Any, E, Nothing] = Validation(_ => Validated.invalid(error))

  def invalidNel[E](error: E): Validation[Any, NonEmptyList[E], Nothing] = Validation(_ => Validated.invalidNel(error))

  def transform[I, O](f: I => O): Validation[I, Nothing, O] = Validation(input => Validated.valid(f(input)))

  def select[I]: SelectBuilder[I] = new SelectBuilder[I]

  final class SelectBuilder[I] {
    def apply[O](f: I => O): Validation[I, Nothing, O] = transform(f)
  }

  def validate[I, E, O](f: I => Validated[E, O], attributes: Validation.Attribute*): Validation[I, E, O] =
    Validation(f, attributes.toList)

  def check[I, E](f: I => Option[E], attributes: Validation.Attribute*): Validation[I, E, Unit] =
    Validation(input => Validated.fromEither(f(input).toLeft(())), attributes.toList)

  // TODO this probably shouldn't spill it's attributes here, what if the input is None?
  // TODO perhaps this could be solved with an optional attributes marker?
  // TODO we need a way to combine errors and assign them to fields?!
  def traverse[F[_]: Traverse, I, E: Semigroup, O](validation: Validation[I, E, O]): Validation[F[I], E, F[O]] =
    Validation(input => input.traverse(validation.run), validation.attributes)
}

sealed abstract private[inspector] class ValidationInstances1 extends ValidationInstances2 {
  implicit def semigroupK[I, E: Semigroup]: SemigroupK[Validation[I, E, *]] = new SemigroupK[Validation[I, E, *]] {
    override def combineK[A](x: Validation[I, E, A], y: Validation[I, E, A]): Validation[I, E, A] = {
      Validation(
        input => SemigroupK[Validated[E, *]].combineK(x.run(input), y.run(input)),
        x.attributes ++ y.attributes
      )
    }
  }

  implicit def arrow[E]: Arrow[Validation[*, E, *]] = new Arrow[Validation[*, E, *]] {
    override def dimap[A, B, C, D](fab: Validation[A, E, B])(f: C => A)(g: B => D): Validation[C, E, D] =
      fab.dimap(f)(g)

    override def lmap[A, B, C](fab: Validation[A, E, B])(f: C => A): Validation[C, E, B] = fab.local(f)

    override def rmap[A, B, C](fab: Validation[A, E, B])(f: B => C): Validation[A, E, C] = fab.map(f)

    override def lift[A, B](f: A => B): Validation[A, E, B] = Validation.transform(f)

    override def first[A, B, C](fa: Validation[A, E, B]): Validation[(A, C), E, (B, C)] =
      Validation[(A, C), E, (B, C)](input => fa.run(input._1).map((_, input._2)))

    override def compose[A, B, C](f: Validation[B, E, C], g: Validation[A, E, B]): Validation[A, E, C] = g.andThen(f)
  }
}

sealed abstract private[inspector] class ValidationInstances2 {
  implicit def applicative[I, E: Semigroup]: CommutativeApplicative[Validation[I, E, *]] =
    new CommutativeApplicative[Validation[I, E, *]] {
      override def pure[A](x: A): Validation[I, E, A] = Validation.valid(x)

      override def ap[A, B](ff: Validation[I, E, A => B])(fa: Validation[I, E, A]): Validation[I, E, B] =
        Validation(input => fa.run(input).ap(ff.run(input)), fa.attributes ++ ff.attributes)
    }

  implicit def semigroup[I, E: Semigroup, O: Semigroup]: Semigroup[Validation[I, E, O]] =
    new Semigroup[Validation[I, E, O]] {
      override def combine(x: Validation[I, E, O], y: Validation[I, E, O]): Validation[I, E, O] =
        Validation(input => x.run(input) combine y.run(input), x.attributes ++ y.attributes)
    }
}

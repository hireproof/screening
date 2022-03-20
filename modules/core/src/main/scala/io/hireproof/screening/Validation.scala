package io.hireproof.screening

import cats.arrow.Arrow
import cats.data.{NonEmptyList, NonEmptyMap, Validated, ValidatedNel}
import cats.syntax.all._
import cats.{Applicative, Semigroup}
import io.hireproof.screening.validations._

import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag

abstract class Validation[-I, +O] {

  /** All constraints that this `Validation` may possibly emit on validation failure */
  def constraints: Set[Constraint]

  def run(input: I): ValidatedNel[Validation.Violation, O]

  final def map[T](f: O => T): Validation[I, T] = Validation[I, T](constraints)(run(_).map(f))

  final def andThen[T](validation: Validation[O, T]): Validation[I, T] =
    Validation(constraints ++ validation.constraints)(run(_).andThen(validation.run))

  final def collect[T](f: PartialFunction[O, T]): Validation[I, T] = map(f.lift).required
}

object Validation {
  implicit final class Ops[I, O](val validation: Validation[I, O]) extends AnyVal {
    def required[T](implicit ev: O =:= Option[T]): Validation[I, T] =
      validation.map(ev.apply).andThen(optional.isDefined)

    def tap: Validation[I, I] = Validation(validation.constraints)(input => validation.run(input).as(input))

    def and[T](right: Validation[I, T]): Validation[I, (O, T)] =
      Validation(validation.constraints ++ right.constraints)(input => (validation.run(input), right.run(input)).tupled)

    def or[OO >: O](right: Validation[I, OO]): Validation[I, OO] =
      Validation(Set(Constraint.Or(validation.constraints, right.constraints))) { input =>
        validation.run(input).orElse(right.run(input))
      }
  }

  final case class Violation(constrain: Constraint, actual: Any)

  final case class Violations(toNem: NonEmptyMap[Selection.History, NonEmptyList[Constraint]]) {
    def modifyHistory(f: Selection.History => Selection.History): Validation.Violations = Violations(toNem.mapKeys(f))

    def modifyConstraints(f: NonEmptyList[Constraint] => NonEmptyList[Constraint]): Validation.Violations =
      Violations(toNem.map(f))

    def modifyConstraint(f: Constraint => Constraint): Validation.Violations = modifyConstraints(_.map(f))

    def merge(errors: Validation.Violations): Validation.Violations = this |+| errors

    def get(history: Selection.History): List[Constraint] = toNem(history).map(_.toList).orEmpty
  }

  object Violations {
    def of(
        head: (Selection.History, NonEmptyList[Constraint]),
        tail: (Selection.History, NonEmptyList[Constraint])*
    ): Validation.Violations = Violations(NonEmptyMap.of(head, tail: _*))

    def ofNel(
        head: (Selection.History, Constraint),
        tail: (Selection.History, Constraint)*
    ): Validation.Violations =
      Violations(NonEmptyMap.of(head.map(NonEmptyList.one), tail.map(_.map(NonEmptyList.one)): _*))

    def one(history: Selection.History, constraints: NonEmptyList[Constraint]): Validation.Violations =
      Violations(NonEmptyMap.one(history, constraints))

    def oneNel(history: Selection.History, constraint: Constraint): Validation.Violations =
      one(history, NonEmptyList.one(constraint))

    def root(errors: NonEmptyList[Constraint]): Validation.Violations = one(Selection.History.Root, errors)

    def rootNel(error: Constraint): Validation.Violations = oneNel(Selection.History.Root, error)

    def fromMap(values: SortedMap[Selection.History, NonEmptyList[Constraint]]): Option[Validation.Violations] =
      NonEmptyMap.fromMap(values).map(Violations.apply)

    implicit val semigroup: Semigroup[Validation.Violations] = new Semigroup[Validation.Violations] {
      override def combine(x: Violations, y: Violations): Violations = Violations(x.toNem |+| y.toNem)
    }
  }

  private def apply[I, O](
      constraints: Set[Constraint]
  )(f: I => ValidatedNel[Validation.Violation, O]): Validation[I, O] = {
    val c = constraints
    new Validation[I, O] {
      override def constraints: Set[Constraint] = c
      override def run(input: I): ValidatedNel[Validation.Violation, O] = f(input)
    }
  }

  def valid[O](value: => O): Validation[Any, O] = Validation(Set.empty)(_ => Validated.valid(value))

  def lift[A, B](f: A => B): Validation[A, B] = Validation(Set.empty)(f(_).valid)

  def invalid(constraints: NonEmptyList[Constraint]): Validation[Any, Unit] =
    Validation(constraints.toList.toSet)(input => Validated.invalid(constraints.map(Violation(_, input))))

  def invalidNel(constraint: Constraint): Validation[Any, Unit] = invalid(NonEmptyList.one(constraint))

  def ask[A]: Validation[A, A] = Validation(Set.empty)(Validated.validNel)

  def not[I](validation: Validation[I, Unit]): Validation[I, Unit] = {
    val constraint = Constraint.Not(validation.constraints)
    Validation(Set(constraint)) { input =>
      validation.run(input) match {
        case Validated.Valid(_)   => Violation(constraint, input).invalidNel
        case Validated.Invalid(_) => ().valid
      }
    }
  }

  def cond[I](constraints: NonEmptyList[Constraint])(f: I => Boolean): Validation[I, Unit] =
    Validation(constraints.toList.toSet)(input => Validated.cond(f(input), (), constraints.map(Violation(_, input))))

  def condNel[I](constraint: Constraint)(f: I => Boolean): Validation[I, Unit] =
    cond(NonEmptyList.one(constraint))(f)

  def fromOption[I, O](constraints: NonEmptyList[Constraint])(f: I => Option[O]): Validation[I, O] =
    Validation(constraints.toList.toSet)(input => f(input).toValid(constraints.map(Violation(_, input))))

  def fromOptionNel[I, O](constraint: Constraint)(f: I => Option[O]): Validation[I, O] =
    fromOption(NonEmptyList.one(constraint))(f)

  def catchOnly[T >: Null <: Throwable]: CatchOnlyBuilder[T] = new CatchOnlyBuilder[T]

  final class CatchOnlyBuilder[T >: Null <: Throwable] {
    def apply[I, O](constraints: NonEmptyList[Constraint])(f: I => O)(implicit tag: ClassTag[T]): Validation[I, O] =
      Validation(constraints.toList.toSet) { input =>
        try f(input).valid
        catch {
          case throwable if tag.runtimeClass.isInstance(throwable) => constraints.map(Violation(_, input)).invalid
        }
      }

    def apply[I, O](constraint: Constraint)(f: I => O)(implicit tag: ClassTag[T]): Validation[I, O] =
      apply(NonEmptyList.one(constraint))(f)
  }

  implicit def applicative[I]: Applicative[Validation[I, *]] = new Applicative[Validation[I, *]] {
    override def pure[A](x: A): Validation[I, A] = Validation.valid(x)
    override def ap[A, B](ff: Validation[I, A => B])(fa: Validation[I, A]): Validation[I, B] =
      Validation(fa.constraints ++ ff.constraints)(i => (ff.run(i), fa.run(i)).mapN(_ apply _))
  }

  implicit val arrow: Arrow[Validation] = new Arrow[Validation] {
    override def lift[A, B](f: A => B): Validation[A, B] = Validation.lift(f)
    override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] =
      Validation(fa.constraints) { case (a, c) => fa.run(a).tupleRight(c) }
    override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] = g.andThen(f)
  }
}

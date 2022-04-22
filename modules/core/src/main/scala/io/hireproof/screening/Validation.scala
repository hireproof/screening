package io.hireproof.screening

import cats.Applicative
import cats.arrow.Arrow
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import io.hireproof.openapi.{Encoder, OpenApi}
import io.hireproof.screening.validations._

import scala.reflect.ClassTag

abstract class Validation[-I, +O] {

  /** All constraints that this `Validation` may possibly emit on validation failure */
  def constraints: Set[Constraint]

  def run(input: I): ValidatedNel[Violation, O]

  final def map[T](f: O => T): Validation[I, T] = Validation[I, T](constraints)(run(_).map(f))

  final def andThen[T](validation: Validation[O, T]): Validation[I, T] =
    Validation(constraints ++ validation.constraints)(run(_).andThen(validation.run))

  final def collect[T](f: PartialFunction[O, T]): Validation[I, T] = map(f.lift).required

  final def modifyConstraint(f: Constraint => Constraint): Validation[I, O] =
    Validation(constraints.map(f))(run(_).leftMap { violations =>
      violations.map {
        case Violation.Validation(constraint, actual) => Violation.Validation(f(constraint), actual)
        case violation                                => violation
      }
    })

  final def withConstraint(constraint: Constraint): Validation[I, O] =
    Validation(Set(constraint))(run(_).leftMap { violations =>
      val actual = violations.collectFirstSome(_.toActual).getOrElse(OpenApi.Null)
      NonEmptyList.one(Violation(constraint, actual))
    })

  def toDebugString: String = constraints.map(_.toDebugString).mkString("[", ", ", "]")
}

object Validation {
  implicit final class Ops[I, O](val validation: Validation[I, O]) extends AnyVal {
    def required[T](implicit ev: O =:= Option[T]): Validation[I, T] =
      validation.map(ev.apply).andThen(optional.isDefined[T])

    def tap: Validation[I, I] = Validation(validation.constraints)(input => validation.run(input).as(input))

    def and[T](right: Validation[I, T]): Validation[I, (O, T)] =
      Validation(validation.constraints ++ right.constraints)(input => (validation.run(input), right.run(input)).tupled)

    def or[OO >: O](right: Validation[I, OO]): Validation[I, OO] =
      Validation(Set(Constraint.Or(validation.constraints, right.constraints))) { input =>
        validation.run(input).orElse(right.run(input))
      }
  }

  private def apply[I, O](constraints: Set[Constraint])(f: I => ValidatedNel[Violation, O]): Validation[I, O] = {
    val c = constraints
    new Validation[I, O] {
      override def constraints: Set[Constraint] = c
      override def run(input: I): ValidatedNel[Violation, O] = f(input)
    }
  }

  def valid[O](value: => O): Validation[Any, O] = Validation(Set.empty)(_ => Validated.valid(value))

  def lift[A, B](f: A => B): Validation[A, B] = Validation(Set.empty)(f(_).valid)

  def invalid(errors: NonEmptyList[Violation]): Validation[Any, Unit] =
    Validation(errors.foldLeft(Set.empty[Constraint])(_ ++ _.toConstraint))(_ => errors.invalid)

  def invalidNel(error: Violation): Validation[Any, Unit] = invalid(NonEmptyList.one(error))

  def ask[A]: Validation[A, A] = Validation(Set.empty)(Validated.validNel)

  def cond[I: Encoder](constraints: NonEmptyList[Constraint])(f: I => Boolean): Validation[I, Unit] =
    Validation(constraints.toList.toSet) { input =>
      Validated.cond(f(input), (), constraints.map(Violation(_, input)))
    }

  def condNel[I: Encoder](constraint: Constraint)(f: I => Boolean): Validation[I, Unit] =
    cond(NonEmptyList.one(constraint))(f)

  def fromOption[I: Encoder, O](constraints: NonEmptyList[Constraint])(f: I => Option[O]): Validation[I, O] =
    Validation(constraints.toList.toSet) { input =>
      f(input).toValid(constraints.map(Violation(_, input)))
    }

  def fromOptionNel[I: Encoder, O](constraint: Constraint)(f: I => Option[O]): Validation[I, O] =
    fromOption(NonEmptyList.one(constraint))(f)

  def option[I, O](validation: Validation[I, O]): Validation[Option[I], Option[O]] =
    Validation(validation.constraints)(_.traverse(validation.run))

  def required[A]: Validation[Option[A], A] = Validation(Set(Constraint.required))(
    Validated.fromOption(_, NonEmptyList.one(Violation(Constraint.required, None)))
  )

  def catchOnly[T >: Null <: Throwable]: CatchOnlyBuilder[T] = new CatchOnlyBuilder[T]

  final class CatchOnlyBuilder[T >: Null <: Throwable] {
    def apply[I: Encoder, O](
        constraints: NonEmptyList[Constraint]
    )(f: I => O)(implicit tag: ClassTag[T]): Validation[I, O] =
      Validation(constraints.toList.toSet) { input =>
        try f(input).valid
        catch {
          case throwable if tag.runtimeClass.isInstance(throwable) => constraints.map(Violation(_, input)).invalid
        }
      }

    def apply[I: Encoder, O](constraint: Constraint)(f: I => O)(implicit tag: ClassTag[T]): Validation[I, O] =
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

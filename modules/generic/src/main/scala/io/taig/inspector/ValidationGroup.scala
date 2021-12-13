package io.taig.inspector

import cats.{Semigroup, Semigroupal, Traverse}
import cats.arrow.Arrow
import cats.data.{NonEmptyList, NonEmptyMap, Validated}
import cats.syntax.all._

abstract class ValidationGroup[-I, O] { self =>
  def run(input: I): Validated[ValidationGroup.Errors, O]

  final def andThen[I1 <: I, T](validation: ValidationGroup[O, T]): ValidationGroup[I1, T] =
    new ValidationGroup[I1, T] {
      override def run(input: I1): Validated[ValidationGroup.Errors, T] = self.run(input).andThen(validation.run)
    }

  final def andThenValidate[I1 <: I, T](f: O => Validated[ValidationGroup.Errors, T]): ValidationGroup[I1, T] =
    new ValidationGroup[I1, T] {
      override def run(input: I1): Validated[ValidationGroup.Errors, T] = self.run(input).andThen(f)
    }
}

object ValidationGroup {
  final case class Errors(values: NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]]) extends AnyVal {
    def prepend(history: Selection.History): Errors = Errors(values.mapKeys(history /:: _))

    def prepend(operation: Selection): Errors = Errors(values.mapKeys(operation /: _))
  }

  object Errors {
    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Errors =
      Errors(NonEmptyMap.one(history, errors))

    def root(errors: NonEmptyList[Validation.Error]): Errors = Errors(NonEmptyMap.one(Selection.History.Root, errors))

    implicit val semigroup: Semigroup[Errors] = new Semigroup[Errors] {
      override def combine(x: Errors, y: Errors): Errors = Errors(x.values combine y.values)
    }
  }

  def pure[A](value: A): ValidationGroup[Any, A] = new ValidationGroup[Any, A] {
    override def run(input: Any): Validated[ValidationGroup.Errors, A] = Validated.valid(value)
  }

  def ask[A]: ValidationGroup[A, A] = new ValidationGroup[A, A] {
    override def run(input: A): Validated[ValidationGroup.Errors, A] = Validated.valid(input)
  }

  def lift[I, O](validation: Validation[I, O]): ValidationGroup[I, O] = new ValidationGroup[I, O] {
    override def run(input: I): Validated[ValidationGroup.Errors, O] = validation.run(input).leftMap(Errors.root)
  }

  def field[I, O](name: String)(validation: ValidationGroup[I, O]): ValidationGroup[I, O] =
    new ValidationGroup[I, O] {
      override def run(input: I): Validated[ValidationGroup.Errors, O] =
        validation.run(input).leftMap(_.prepend(Selection.Field(name)))
    }

  def index[I, O](value: Int)(validation: ValidationGroup[I, O]): ValidationGroup[I, O] =
    new ValidationGroup[I, O] {
      override def run(input: I): Validated[ValidationGroup.Errors, O] =
        validation.run(input).leftMap(_.prepend(Selection.Index(value)))
    }

  def collection[F[_]: Traverse, I, O](validation: ValidationGroup[I, O]): ValidationGroup[F[I], F[O]] =
    new ValidationGroup[F[I], F[O]] {
      override def run(input: F[I]): Validated[ValidationGroup.Errors, F[O]] =
        input.zipWithIndex.traverse { case (input, value) => index(value)(validation).run(input) }
    }

  def option[I, O](validation: ValidationGroup[I, O]): ValidationGroup[Option[I], Option[O]] =
    new ValidationGroup[Option[I], Option[O]] {
      override def run(input: Option[I]): Validated[ValidationGroup.Errors, Option[O]] =
        input.traverse(validation.run)
    }

  implicit val arrow: Arrow[ValidationGroup] = new Arrow[ValidationGroup] {
    override def lift[A, B](f: A => B): ValidationGroup[A, B] = new ValidationGroup[A, B] {
      override def run(input: A): Validated[ValidationGroup.Errors, B] = Validated.valid(f(input))
    }

    override def first[A, B, C](fa: ValidationGroup[A, B]): ValidationGroup[(A, C), (B, C)] =
      new ValidationGroup[(A, C), (B, C)] {
        override def run(input: (A, C)): Validated[ValidationGroup.Errors, (B, C)] =
          fa.run(input._1).map((_, input._2))
      }

    override def compose[A, B, C](f: ValidationGroup[B, C], g: ValidationGroup[A, B]): ValidationGroup[A, C] =
      new ValidationGroup[A, C] {
        override def run(input: A): Validated[ValidationGroup.Errors, C] = g.run(input).andThen(f.run)
      }
  }

  implicit def semigroupal[I]: Semigroupal[ValidationGroup[I, *]] = new Semigroupal[ValidationGroup[I, *]] {
    override def product[A, B](fa: ValidationGroup[I, A], fb: ValidationGroup[I, B]): ValidationGroup[I, (A, B)] =
      new ValidationGroup[I, (A, B)] {
        override def run(input: I): Validated[Errors, (A, B)] = fa.run(input).product(fb.run(input))
      }
  }
}

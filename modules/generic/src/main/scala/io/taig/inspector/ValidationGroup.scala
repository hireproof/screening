package io.taig.inspector

import cats.arrow.Arrow
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import cats.{Semigroup, Semigroupal, Traverse}

import scala.collection.immutable.HashMap

abstract class ValidationGroup[-I, O] { self =>
  def run(input: I): Validated[ValidationGroup.Errors, O]

  final def map[T](f: O => T): ValidationGroup[I, T] = new ValidationGroup[I, T] {
    override def run(input: I): Validated[ValidationGroup.Errors, T] = self.run(input).map(f)
  }

  final def downField[T](name: String, select: O => T): ValidationGroup[I, T] = new ValidationGroup[I, T] {
    override def run(input: I): Validated[ValidationGroup.Errors, T] =
      self.run(input).map(select).leftMap(_.prepend(Selection.Field(name)))
  }

  final def downBranch[T](name: String, select: PartialFunction[O, T]): ValidationGroup[I, T] =
    new ValidationGroup[I, T] {
      override def run(input: I): Validated[ValidationGroup.Errors, T] =
        self
          .run(input)
          .map(select.lift)
          .leftMap(_.prepend(Selection.Field(name)))
          .andThen {
            case Some(value) => Validated.valid(value)
            case None =>
              Validated.invalid(ValidationGroup.Errors.root(NonEmptyList.one(Validation.Error.Unknown(name))))
          }
    }

  final def andThen[T](group: ValidationGroup[O, T]): ValidationGroup[I, T] =
    new ValidationGroup[I, T] {
      override def run(input: I): Validated[ValidationGroup.Errors, T] = self.run(input).andThen(group.run)
    }

  final def andThenRun[T](f: O => Validated[ValidationGroup.Errors, T]): ValidationGroup[I, T] =
    new ValidationGroup[I, T] {
      override def run(input: I): Validated[ValidationGroup.Errors, T] = self.run(input).andThen(f)
    }

  final def andThenValidation[T](validation: Validation[O, T]): ValidationGroup[I, T] =
    andThen(ValidationGroup.lift(validation))

  final def andThenValidationRun[T](f: O => ValidatedNel[Validation.Error, T]): ValidationGroup[I, T] =
    new ValidationGroup[I, T] {
      override def run(input: I): Validated[ValidationGroup.Errors, T] =
        self.run(input).andThen(f(_).leftMap(ValidationGroup.Errors.root))
    }

  final def or[II <: I](validation: ValidationGroup[II, O]): ValidationGroup[II, O] = new ValidationGroup[II, O] {
    override def run(input: II): Validated[ValidationGroup.Errors, O] =
      self.run(input).orElse(validation.run(input))
  }
}

object ValidationGroup {
  final class Errors private (
      head: (Selection.History, NonEmptyList[Validation.Error]),
      tail: HashMap[Selection.History, NonEmptyList[Validation.Error]]
  ) {
    def prepend(selection: Selection): Errors = new Errors(
      head.leftMap(selection +: _),
      tail.map { case (history, errors) => (selection +: history, errors) }
    )

    def merge(errors: Errors): Errors = {
      val lookup = errors.toMap

      val result = errors.keys.foldLeft(this.toMap) { case (result, history) =>
        result.updatedWith(history) {
          case Some(x) => Some(x concatNel lookup(history))
          case None    => Some(lookup(history))
        }
      }

      Errors.unsafeFromMap(result)
    }

    def toMap: HashMap[Selection.History, NonEmptyList[Validation.Error]] = tail + head

    def keys: Iterable[Selection.History] = toMap.keys
  }

  object Errors {
    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Errors =
      new Errors(history -> errors, HashMap.empty)

    def root(errors: NonEmptyList[Validation.Error]): Errors = one(Selection.History.Root, errors)

    def fromMap(values: Map[Selection.History, NonEmptyList[Validation.Error]]): Option[Errors] =
      Option.when(values.nonEmpty)(new Errors(values.head, values.tail.to(HashMap)))

    def unsafeFromMap(values: Map[Selection.History, NonEmptyList[Validation.Error]]): Errors = fromMap(values).get

    def fromErrors(history: Selection.History, error: Validation.Error, errors: Validation.Error*): Errors =
      one(history, NonEmptyList(error, errors.toList))

    implicit val semigroup: Semigroup[Errors] = new Semigroup[Errors] {
      override def combine(x: Errors, y: Errors): Errors = x merge y
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

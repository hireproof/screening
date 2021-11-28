package io.taig

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._

package object inspector {
  type ValidationNel[-I, +E, O] = Validation[I, NonEmptyList[E], O]

  object ValidationNel {
    def validate[I, E, O](f: I => ValidatedNel[E, O], attributes: Validation.Attribute*): ValidationNel[I, E, O] =
      Validation(f, attributes.toList)

    def check[I, E](f: I => Option[E], attributes: Validation.Attribute*): ValidationNel[I, E, Unit] =
      Validation(input => Validated.fromEither(f(input).map(NonEmptyList.one).toLeft(())), attributes.toList)

    def checkAll[I, E](f: I => List[E], attributes: Validation.Attribute*): ValidationNel[I, E, Unit] =
      Validation(input => Validated.fromEither(NonEmptyList.fromList(f(input)).toLeft(())), attributes.toList)
  }

  implicit class RichValidatedNel[E, A](val validated: ValidatedNel[E, A]) extends AnyVal {
    def errors: List[E] = validated.fold(_.toList, _ => Nil)

    def error: Option[E] = validated.fold(_.head.some, _ => none)
  }
}

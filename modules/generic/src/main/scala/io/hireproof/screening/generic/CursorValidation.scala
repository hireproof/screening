package io.hireproof.screening.generic

import cats.data.Validated

abstract class CursorValidation[I, O] {
  def apply(cursor: Cursor[Identity, I]): Validated[Cursor.Errors, O]

  final def run(input: I): Validated[Cursor.Errors, O] = apply(Cursor.root(input))
}

object CursorValidation {
  def apply[I, O](f: Cursor[Identity, I] => Cursor[Identity, O]): CursorValidation[I, O] =
    new CursorValidation[I, O] {
      override def apply(cursor: Cursor[Identity, I]): Validated[Cursor.Errors, O] = f(cursor).toValidated
    }
}

package io.hireproof.screening.generic

import cats.data.Validated

abstract class CursorValidation[-I, +O] {
  def apply(cursor: Cursor.Root[I]): Cursor.Root[O]

  final def run(input: I): Validated[Cursor.Errors, O] = apply(Cursor.root(input)).toValidated
}

object CursorValidation {
  def apply[I, O](f: Cursor.Root[I] => Cursor.Root[O]): CursorValidation[I, O] =
    new CursorValidation[I, O] {
      override def apply(cursor: Cursor.Root[I]): Cursor.Root[O] = f(cursor)
    }

  def oneOf[I, O](f: I => (String, Validated[Cursor.Errors, O])): CursorValidation[I, O] =
    CursorValidation[I, O](_.oneOf(f).run)
}

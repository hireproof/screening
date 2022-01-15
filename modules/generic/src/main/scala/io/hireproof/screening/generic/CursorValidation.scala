package io.hireproof.screening.generic

import cats.data.Validated
import io.hireproof.screening.Validation
import io.hireproof.screening.generic.Cursor.Root

abstract class CursorValidation[-I, +O] {
  def apply(cursor: Cursor.Root[I]): Validated[Validation.Errors, O]

  final def run(input: I): Validated[Validation.Errors, O] = apply(Cursor.root(input))
}

object CursorValidation {
  def apply[I, O](f: Cursor.Root[I] => Validated[Validation.Errors, O]): CursorValidation[I, O] =
    new CursorValidation[I, O] {
      override def apply(cursor: Root[I]): Validated[Validation.Errors, O] = f(cursor)
    }

  def oneOf[I, O](f: I => (String, Validated[Validation.Errors, O])): CursorValidation[I, O] =
    CursorValidation[I, O](_.oneOf(f).run)
}

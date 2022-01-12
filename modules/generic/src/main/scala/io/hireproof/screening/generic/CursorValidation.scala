package io.hireproof.screening.generic

import cats.data.Validated
import io.hireproof.screening.generic.Cursor.Root

abstract class CursorValidation[-I, +O] {
  def apply(cursor: Cursor.Root[I]): Validated[Cursor.Errors, O]

  final def run(input: I): Validated[Cursor.Errors, O] = apply(Cursor.root(input))
}

object CursorValidation {
  def apply[I, O](f: Cursor.Root[I] => Validated[Cursor.Errors, O]): CursorValidation[I, O] =
    new CursorValidation[I, O] {
      override def apply(cursor: Root[I]): Validated[Cursor.Errors, O] = f(cursor)
    }

  def oneOf[I, O](f: I => (String, Validated[Cursor.Errors, O])): CursorValidation[I, O] =
    CursorValidation[I, O](_.oneOf(f).run)
}

package io.hireproof.screening.generic

import cats.data.Validated

abstract class CursorValidation[-I, +O] {
  def apply(cursor: Cursor[Identity, I]): Cursor[Identity, O]

  final def run(input: I): Validated[Cursor.Errors, O] = apply(Cursor.root(input)).toValidated
}

object CursorValidation {
  def apply[I, O](f: Cursor[Identity, I] => Cursor[Identity, O]): CursorValidation[I, O] =
    new CursorValidation[I, O] {
      override def apply(cursor: Cursor[Identity, I]): Cursor[Identity, O] = f(cursor)
    }

  def oneOf[A, B](f: A => (String, Validated[Cursor.Errors, B])): CursorValidation[A, B] =
    CursorValidation[A, B](_.oneOf(f).run)
}

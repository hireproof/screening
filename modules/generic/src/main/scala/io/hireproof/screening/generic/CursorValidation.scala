package io.hireproof.screening.generic

import cats.Id
import cats.data.Validated

abstract class CursorValidation[A, B] { self =>
  def get(cursor: Cursor[Id, A]): Cursor.Result[Id, B]

  final def run(input: A): Validated[Cursor.Errors, B] = get(Cursor.root(input)).toValidated
}

object CursorValidation {
  def apply[A, B](f: Cursor[Id, A] => Cursor.Result[Id, B]): CursorValidation[A, B] = new CursorValidation[A, B] {
    override def get(cursor: Cursor[Id, A]): Cursor.Result[Id, B] = f(cursor)
  }

  def oneOf[A, B](f: A => (String, Validated[Cursor.Errors, B])): CursorValidation[A, B] =
    CursorValidation[A, B](_.oneOf(f).run)
}

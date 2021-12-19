package io.taig.inspector

import cats.Id
import cats.data.Validated
import io.taig.inspector.Cursor.Result

abstract class CursorValidation[I, O] {
  def apply(cursor: Cursor[Id, I]): Validated[Cursor.Failure, O]

  def run(input: I): Validated[Cursor.Failure, O] = apply(Cursor.root(input))
}

object CursorValidation {
  def apply[I, O](f: Cursor[Id, I] => Validated[Cursor.Failure, O]): CursorValidation[I, O] =
    new CursorValidation[I, O] {
      override def apply(cursor: Cursor[Id, I]): Validated[Cursor.Failure, O] = f(cursor)
    }
}

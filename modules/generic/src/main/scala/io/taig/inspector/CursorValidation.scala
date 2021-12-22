package io.taig.inspector

import cats.Id

abstract class CursorValidation[I, O] {
  def apply(cursor: Cursor[Id, I]): Cursor.Result[Id, O]

  def run(input: I): Cursor.Result[Id, O] = apply(Cursor.root(input))
}

object CursorValidation {
  def apply[I, O](f: Cursor[Id, I] => Cursor.Result[Id, O]): CursorValidation[I, O] = new CursorValidation[I, O] {
    override def apply(cursor: Cursor[Id, I]): Cursor.Result[Id, O] = f(cursor)
  }
}

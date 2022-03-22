package io.hireproof.screening

import cats.Show
import cats.syntax.all._

final case class Actual(value: String) extends AnyVal

object Actual {
  def fromShow[A: Show](value: A): Actual = Actual(value.show)

  def fromAny(value: Any): Actual = Actual(anyToString(value))
}

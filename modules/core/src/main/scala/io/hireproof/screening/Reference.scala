package io.hireproof.screening

import cats.Show
import cats.syntax.all._

import scala.Numeric.Implicits._

final case class Reference(value: String) extends AnyVal

object Reference {
  def fromAny(value: Any): Reference = Reference(value.toString)

  def fromShow[A: Show](value: A): Reference = Reference(value.show)

  def fromNumeric[A: Numeric](value: A): Reference = {
    val double = value.toDouble
    Reference(String.format(if (double % 1.0d != 0) "%s" else "%.0f", double))
  }

  def fromIterable[A: Show](values: Iterable[A]): Reference = Reference(values.map(_.show).mkString("[", ", ", "]"))
}

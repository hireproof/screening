package io.taig.inspector

import cats.Order
import cats.data.Chain

final case class Cursor(values: Chain[Cursor.Operation]) extends AnyVal {
  def /:(operation: Cursor.Operation): Cursor = Cursor(operation +: values)

  def /::(cursor: Cursor): Cursor = Cursor(cursor.values ++ values)
}

object Cursor {
  val Root: Cursor = Cursor(Chain.empty)

  sealed abstract class Operation extends Product with Serializable

  object Operation {
    final case class Field(name: String) extends Operation
    final case class Index(value: Int) extends Operation

    implicit val order: Order[Operation] = new Order[Operation] {
      override def compare(x: Operation, y: Operation): Int = (x, y) match {
        case (Field(x), Field(y)) => x compare y
        case (Index(x), Index(y)) => x compare y
        case (Field(_), Index(_)) => 1
        case (Index(_), Field(_)) => -1
      }
    }
  }

  implicit val order: Order[Cursor] = Order.by(_.values)
}

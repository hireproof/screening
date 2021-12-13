package io.taig.inspector

import cats.Order

sealed abstract class Selection extends Product with Serializable

object Selection {
  final case class Field(name: String) extends Selection
  final case class Index(value: Int) extends Selection

  implicit val order: Order[Selection] = new Order[Selection] {
    override def compare(x: Selection, y: Selection): Int = (x, y) match {
      case (Field(x), Field(y)) => x compare y
      case (Index(x), Index(y)) => x compare y
      case (Field(_), Index(_)) => 1
      case (Index(_), Field(_)) => -1
    }
  }

  final case class History(values: List[Selection]) extends AnyVal {
    def /:(operation: Selection): History = History(operation +: values)

    def /::(cursor: History): History = History(cursor.values ++ values)

    def toJsonPath: String = values.foldLeft("") {
      case (result, Selection.Field(name))  => s".$name$result"
      case (result, Selection.Index(index)) => s"[$index]$result"
    }

    override def toString: String = toJsonPath
  }

  object History {
    val Root: History = History(Nil)

    def from(selection: Iterable[Selection]): History = History(selection.toList)

    def of(selection: Selection*): History = from(selection)

    implicit val order: Order[History] = Order.by(_.values)
  }
}

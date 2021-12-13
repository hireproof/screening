package io.taig.inspector

sealed abstract class Selection extends Product with Serializable

object Selection {
  final case class Field(name: String) extends Selection
  final case class Index(value: Int) extends Selection

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
  }
}

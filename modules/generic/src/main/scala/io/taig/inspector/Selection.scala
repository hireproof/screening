package io.taig.inspector

import cats.Order
import cats.syntax.all._

import java.util.regex.Pattern

sealed abstract class Selection extends Product with Serializable

object Selection {
  final case class Field(name: String) extends Selection
  final case class Index(value: Int) extends Selection

  final case class History(values: List[Selection]) extends AnyVal {
    def ::(field: String): History = History(Field(field) :: values)

    def ::(index: Int): History = History(Index(index) :: values)

    def ::(operation: Selection): History = History(operation :: values)

    def ++(history: History): History = History(values ++ history.values)

    def toJsonPath: String = if (values.isEmpty) "."
    else
      values.foldLeft("") {
        case (result, Selection.Field(name))  => s".$name$result"
        case (result, Selection.Index(index)) => s"[$index]$result"
      }

    override def toString: String = toJsonPath
  }

  object History {
    val Root: Selection.History = History(Nil)

    private val Parser = Pattern.compile("(?:\\.(\\w+))|\\[(\\d+)\\]")

    def parse(value: String): Either[String, Selection.History] =
      if (value.isEmpty) Left("Empty")
      else if (value == ".") Right(Selection.History.Root)
      else {
        val result = List.newBuilder[Selection]
        val matcher = Parser.matcher(value)
        var lastOffset = 0

        try {
          while (matcher.find()) {
            val field = matcher.group(1)
            val index = matcher.group(2)

            if (matcher.start() > lastOffset) throw new IllegalArgumentException("Contains invalid characters")
            else lastOffset = matcher.end()

            if (field != null) result += Selection.Field(field)
            else result += Selection.Index(index.toInt)
          }

          if (lastOffset < value.length) throw new IllegalArgumentException("Contains invalid characters")

          History(result.result().reverse).asRight
        } catch {
          case _: NumberFormatException            => "Invalid index format".asLeft
          case exception: IllegalArgumentException => exception.getMessage.asLeft
        }
      }

    implicit val order: Order[Selection.History] = Order.by(_.values)
  }

  implicit val order: Order[Selection] = new Order[Selection] {
    override def compare(x: Selection, y: Selection): Int = (x, y) match {
      case (Field(x), Field(y)) => x compare y
      case (Index(x), Index(y)) => x compare y
      case (Field(_), Index(y)) => 1
      case (Index(_), Field(_)) => -1
    }
  }
}

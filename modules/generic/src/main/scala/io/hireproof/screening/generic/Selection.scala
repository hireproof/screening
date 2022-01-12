package io.hireproof.screening.generic

import cats.Order
import cats.data.Chain
import cats.syntax.all._

import java.util.regex.Pattern

sealed abstract class Selection extends Product with Serializable

object Selection {
  final case class Field(name: String) extends Selection
  final case class Index(value: Int) extends Selection

  final case class History(toChain: Chain[Selection]) extends AnyVal {
    def /(field: String): History = append(Field(field))

    def /(index: Int): History = append(Index(index))

    def /(selection: Selection): History = append(selection)

    def append(selection: Selection): History = History(toChain append selection)

    def /:(field: String): History = prepend(Field(field))

    def /:(index: Int): History = prepend(Index(index))

    def /:(selection: Selection): History = prepend(selection)

    def prepend(selection: Selection): History = History(toChain prepend selection)

    def ++(history: History): History = History(toChain ++ history.toChain)

    def isRoot: Boolean = toChain.isEmpty

    def up: Selection.History = toChain.initLast match {
      case Some((init, _)) => History(init)
      case None            => History.Root
    }

    def toList: List[Selection] = toChain.toList

    def toJsonPath: String = if (toChain.isEmpty) "."
    else
      toChain.foldLeft("") {
        case (result, Selection.Field(name))  => s"$result.$name"
        case (result, Selection.Index(index)) => s"$result[$index]"
      }

    override def toString: String = toJsonPath
  }

  object History {
    private val Parser = Pattern.compile("(?:\\.(\\w+))|\\[(\\d+)\\]")

    val Root: Selection.History = History(Chain.empty)

    def parse(value: String): Either[String, Selection.History] =
      if (value.isEmpty) Left("Empty")
      else if (value == ".") Right(Root)
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

          History(Chain.fromSeq(result.result())).asRight
        } catch {
          case _: NumberFormatException            => "Invalid index format".asLeft
          case exception: IllegalArgumentException => exception.getMessage.asLeft
        }
      }

    implicit val order: Order[Selection.History] = Order.by(_.toChain)
  }

  implicit val order: Order[Selection] = new Order[Selection] {
    override def compare(x: Selection, y: Selection): Int = (x, y) match {
      case (Field(x), Field(y)) => x compare y
      case (Index(x), Index(y)) => x compare y
      case (Field(_), Index(_)) => 1
      case (Index(_), Field(_)) => -1
    }
  }
}

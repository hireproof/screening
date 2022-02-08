package io.hireproof.screening

import cats.Show

private[screening] case class Symbol(value: Int) {
  def next: Symbol = Symbol(value + 1)

  override def toString: String = Symbol.mapping(value).toString
}

private[screening] object Symbol {
  private val mapping: Int => Char = {
    case 0  => 'a'
    case 1  => 'b'
    case 2  => 'c'
    case 3  => 'd'
    case 4  => 'e'
    case 5  => 'f'
    case 6  => 'g'
    case 7  => 'h'
    case 8  => 'i'
    case 9  => 'j'
    case 10 => 'k'
    case 11 => 'l'
    case 12 => 'm'
    case 13 => 'n'
    case 14 => 'o'
    case 15 => 'p'
    case 16 => 'q'
    case 17 => 'r'
    case 18 => 's'
    case 19 => 't'
    case 20 => 'u'
    case 21 => 'v'
    case 22 => 'w'
    case 23 => 'x'
    case 24 => 'y'
    case 25 => 'z'
    case _  => '?'
  }

  val Default: Symbol = Symbol(0)

  implicit val show: Show[Symbol] = Show.fromToString
}

package io.hireproof.screening

import cats.Semigroup
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

final case class Violations(toNem: NonEmptyMap[Selection.History, NonEmptyList[Violation]]) {
  def modifyHistory(f: Selection.History => Selection.History): Violations = Violations(toNem.mapKeys(f))

  def modifyErrors(f: NonEmptyList[Violation] => NonEmptyList[Violation]): Violations = Violations(toNem.map(f))

  def modifyError(f: Violation => Violation): Violations = modifyErrors(_.map(f))

  def merge(violations: Violations): Violations = this |+| violations

  def get(history: Selection.History): List[Violation] = toNem(history).map(_.toList).orEmpty

  def head(history: Selection.History): Option[Violation] = toNem(history).map(_.head)
}

object Violations {
  def of(
      head: (Selection.History, NonEmptyList[Violation]),
      tail: (Selection.History, NonEmptyList[Violation])*
  ): Violations =
    Violations(NonEmptyMap.of(head, tail: _*))

  def ofNel(head: (Selection.History, Violation), tail: (Selection.History, Violation)*): Violations =
    Violations(NonEmptyMap.of(head.map(NonEmptyList.one), tail.map(_.map(NonEmptyList.one)): _*))

  def one(history: Selection.History, violations: NonEmptyList[Violation]): Violations =
    Violations(NonEmptyMap.one(history, violations))

  def oneNel(history: Selection.History, violation: Violation): Violations = one(history, NonEmptyList.one(violation))

  def root(violations: NonEmptyList[Violation]): Violations = one(Selection.History.Root, violations)

  def rootNel(violation: Violation): Violations = oneNel(Selection.History.Root, violation)

  def fromMap(values: SortedMap[Selection.History, NonEmptyList[Violation]]): Option[Violations] =
    NonEmptyMap.fromMap(values).map(Violations.apply)

  implicit val semigroup: Semigroup[Violations] = new Semigroup[Violations] {
    override def combine(x: Violations, y: Violations): Violations = x merge y
  }
}

package io.hireproof.screening

import cats.Semigroup
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

final case class Violations(toNem: NonEmptyMap[Selection.History, NonEmptyList[Constraint]]) {
  def modifyHistory(f: Selection.History => Selection.History): Violations = Violations(toNem.mapKeys(f))

  def modifyConstraints(f: NonEmptyList[Constraint] => NonEmptyList[Constraint]): Violations =
    Violations(toNem.map(f))

  def modifyConstraint(f: Constraint => Constraint): Violations = modifyConstraints(_.map(f))

  def merge(errors: Violations): Violations = this |+| errors

  def get(history: Selection.History): List[Constraint] = toNem(history).map(_.toList).orEmpty
}

object Violations {
  def of(
      head: (Selection.History, NonEmptyList[Constraint]),
      tail: (Selection.History, NonEmptyList[Constraint])*
  ): Violations = Violations(NonEmptyMap.of(head, tail: _*))

  def ofNel(
      head: (Selection.History, Constraint),
      tail: (Selection.History, Constraint)*
  ): Violations =
    Violations(NonEmptyMap.of(head.map(NonEmptyList.one), tail.map(_.map(NonEmptyList.one)): _*))

  def one(history: Selection.History, constraints: NonEmptyList[Constraint]): Violations =
    Violations(NonEmptyMap.one(history, constraints))

  def oneNel(history: Selection.History, constraint: Constraint): Violations =
    one(history, NonEmptyList.one(constraint))

  def root(errors: NonEmptyList[Constraint]): Violations = one(Selection.History.Root, errors)

  def rootNel(error: Constraint): Violations = oneNel(Selection.History.Root, error)

  def fromMap(values: SortedMap[Selection.History, NonEmptyList[Constraint]]): Option[Violations] =
    NonEmptyMap.fromMap(values).map(Violations.apply)

  implicit val semigroup: Semigroup[Violations] = new Semigroup[Violations] {
    override def combine(x: Violations, y: Violations): Violations = Violations(x.toNem |+| y.toNem)
  }
}

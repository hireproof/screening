package io.hireproof.screening

import cats.{Semigroup, SemigroupK}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

final case class Violations[A](toNem: NonEmptyMap[Selection.History, NonEmptyList[A]]) {
  def modifyHistory(f: Selection.History => Selection.History): Violations[A] = Violations(toNem.mapKeys(f))

  def modifyValues[T](f: NonEmptyList[A] => NonEmptyList[T]): Violations[T] = Violations(toNem.map(f))

  def map[T](f: A => T): Violations[T] = modifyValues(_.map(f))

  def merge(errors: Violations[A]): Violations[A] = this <+> errors

  def get(history: Selection.History): List[A] = toNem(history).map(_.toList).orEmpty
}

object Violations {
  def of[A](head: (Selection.History, NonEmptyList[A]), tail: (Selection.History, NonEmptyList[A])*): Violations[A] =
    Violations(NonEmptyMap.of(head, tail: _*))

  def ofNel[A](head: (Selection.History, A), tail: (Selection.History, A)*): Violations[A] =
    Violations(NonEmptyMap.of(head.map(NonEmptyList.one), tail.map(_.map(NonEmptyList.one)): _*))

  def one[A](history: Selection.History, values: NonEmptyList[A]): Violations[A] =
    Violations(NonEmptyMap.one(history, values))

  def oneNel[A](history: Selection.History, value: A): Violations[A] = one(history, NonEmptyList.one(value))

  def root[A](errors: NonEmptyList[A]): Violations[A] = one(Selection.History.Root, errors)

  def rootNel[A](error: A): Violations[A] = oneNel(Selection.History.Root, error)

  def fromMap[A](values: SortedMap[Selection.History, NonEmptyList[A]]): Option[Violations[A]] =
    NonEmptyMap.fromMap(values).map(Violations[A])

  implicit val semigroupK: SemigroupK[Violations] = new SemigroupK[Violations] {
    override def combineK[A](x: Violations[A], y: Violations[A]): Violations[A] = Violations(x.toNem |+| y.toNem)
  }

  implicit def semigroup[A]: Semigroup[Violations[A]] = semigroupK.algebra[A]
}

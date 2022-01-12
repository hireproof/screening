package io.hireproof.screening.generic

import cats._
import cats.data.{NonEmptyList, NonEmptyMap, Validated, ValidatedNel}
import cats.syntax.all._
import io.hireproof.screening.Validation

import scala.collection.immutable.SortedMap

sealed abstract class Cursor[+F[+_], +A] { self =>
  def modifyHistory(f: Selection.History => Selection.History): Cursor[F, A]

  def run: Cursor[Identity, F[A]]

  final def runWith[T](validation: Validation[A, T]): Cursor[Identity, F[T]] = validate(validation).run

  final def toValidated: Validated[Cursor.Errors, F[A]] = run match {
    case Cursor.Success(value: Cursor.Value[F[A]]) => Validated.valid(value.value)
    case Cursor.Failure(errors)                    => Validated.invalid(errors)
  }

  def map[T](f: A => T): Cursor[F, T]

  def andThen[T](f: A => Cursor[Identity, T]): Cursor[F, T]

  def andThen[T](validation: CursorValidation[A, T]): Cursor[F, T]

  def validate[T](validation: Validation[A, T]): Cursor[F, T]

  final def ensure[T](validation: Validation[A, Unit]): Cursor[F, A] = validate(validation.tap)

  def oneOf[T](f: A => (String, Validated[Cursor.Errors, T])): Cursor[F, T]

  def unnamedOneOf[T](f: A => Validated[Cursor.Errors, T]): Cursor[F, T]

  def field[T](name: String, select: A => T): Cursor[F, T]

  def option[T](implicit ev: A <:< Option[T]): Cursor[λ[`+a` => F[Option[a]]], T]

  final def option[B](name: String, select: A => Option[B]): Cursor[λ[`+a` => F[Option[a]]], B] =
    field(name, select).option

  def collection[G[+_], B](implicit ev: A <:< G[B], G: Traverse[G]): Cursor[λ[`+a` => F[G[a]]], B]

  final def collection[G[+_]: Traverse, T](name: String, select: A => G[T]): Cursor[λ[`+a` => F[G[a]]], T] =
    field(name, select).collection

  def unindexedCollection[G[+_], T](implicit ev: A <:< G[T], G: Traverse[G]): Cursor[λ[`+a` => F[G[a]]], T]
}

object Cursor {
  final case class Value[+A](history: Selection.History, value: A) {
    def modifyHistory(f: Selection.History => Selection.History): Cursor.Value[A] = copy(history = f(history))

    def map[B](f: A => B): Cursor.Value[B] = copy(value = f(value))
  }

  final case class Errors(toNem: NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]]) {
    def modifyHistory(f: Selection.History => Selection.History): Cursor.Errors = Cursor.Errors(toNem.mapKeys(f))

    def modifyError(f: NonEmptyList[Validation.Error] => NonEmptyList[Validation.Error]): Cursor.Errors = Errors(
      toNem.map(f)
    )

    def modifyErrors(f: Validation.Error => Validation.Error): Cursor.Errors = modifyError(_.map(f))

    def merge(errors: Cursor.Errors): Cursor.Errors = this |+| errors

    def get(history: Selection.History): List[Validation.Error] = toNem(history).map(_.toList).orEmpty
  }

  object Errors {
    def ofErrors(
        head: (Selection.History, NonEmptyList[Validation.Error]),
        tail: (Selection.History, NonEmptyList[Validation.Error])*
    ): Cursor.Errors = Errors(NonEmptyMap.of(head, tail: _*))

    def ofError(
        head: (Selection.History, Validation.Error),
        tail: (Selection.History, Validation.Error)*
    ): Cursor.Errors =
      Errors(NonEmptyMap.of(head.map(NonEmptyList.one), tail.map(_.map(NonEmptyList.one)): _*))

    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Cursor.Errors =
      Errors(NonEmptyMap.one(history, errors))

    def oneNel(history: Selection.History, error: Validation.Error): Cursor.Errors =
      one(history, NonEmptyList.one(error))

    def root(errors: NonEmptyList[Validation.Error]): Cursor.Errors = one(Selection.History.Root, errors)

    def rootNel(error: Validation.Error): Cursor.Errors = oneNel(Selection.History.Root, error)

    def fromMap(values: SortedMap[Selection.History, NonEmptyList[Validation.Error]]): Option[Cursor.Errors] =
      NonEmptyMap.fromMap(values).map(Errors.apply)

    implicit val semigroup: Semigroup[Cursor.Errors] = new Semigroup[Cursor.Errors] {
      override def combine(x: Errors, y: Errors): Errors = Errors(x.toNem |+| y.toNem)
    }
  }

  final case class Success[F[+_], O](value: F[Cursor.Value[O]])(implicit F: Traverse[F]) extends Cursor[F, O] {
    override def modifyHistory(f: Selection.History => Selection.History): Cursor[F, O] =
      Success[F, O](value.map(_.modifyHistory(f)))

    override def run: Cursor[Identity, F[O]] =
      F.traverse[Cursor[Identity, *], Value[O], O](value) { case Cursor.Value(history, o) =>
        Cursor.Success[Identity, O](Cursor.Value(history, o))
      }

    override def map[T](f: O => T): Cursor[F, T] = Success(value.map(_.map(f)))

    override def andThen[T](f: O => Cursor[Identity, T]): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, o) => f(o).modifyHistory(history ++ _) } match {
        case Success(value: Cursor.Value[F[T]]) => Success(value.value.map(Cursor.Value(value.history, _)))
        case cursor: Failure                    => cursor
      }

    override def andThen[T](validation: CursorValidation[O, T]): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, o) => validation.apply(Cursor.withHistory(history, o)) } match {
        case Success(value: Cursor.Value[F[T]]) => Success(value.value.map(Cursor.Value(value.history, _)))
        case cursor: Failure                    => cursor
      }

    override def validate[T](validation: Validation[O, T]): Cursor[F, T] = {
      F.traverse(value) { case Cursor.Value(history, a) =>
        validation
          .run(a)
          .leftMap(Cursor.Errors.one(history, _))
          .map(b => Cursor.Value(history, b))
      }.fold(Cursor.Failure.apply, Cursor.Success[F, T])
    }

    override def field[T](name: String, select: O => T): Cursor[F, T] =
      Success(value.map(_.map(select))).modifyHistory(_ / name)

    override def oneOf[T](f: O => (String, Validated[Errors, T])): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, a) =>
        val (name, result) = f(a)
        result.leftMap(_.modifyHistory(history / name ++ _)).map(Cursor.Value(history / name, _))
      }.fold(Failure.apply, Success[F, T])

    override def unnamedOneOf[T](f: O => Validated[Errors, T]): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, a) =>
        f(a).leftMap(_.modifyHistory(history ++ _)).map(Cursor.Value(history, _))
      }.fold(Failure.apply, Success[F, T])

    override def option[T](implicit ev: O <:< Option[T]): Cursor[λ[`+a` => F[Option[a]]], T] =
      Success[λ[`+a` => F[Option[a]]], T](value.map { case Cursor.Value(history, o) =>
        ev.apply(o).map(Cursor.Value(history, _))
      })(F.compose[Option])

    override def collection[G[+_], T](implicit ev: O <:< G[T], G: Traverse[G]): Cursor[λ[`+a` => F[G[a]]], T] =
      Success[λ[`+a` => F[G[a]]], T](value.map { case Cursor.Value(history, o) =>
        ev.apply(o).mapWithIndex((b, index) => Cursor.Value(history / index, b))
      })(F.compose(G))

    override def unindexedCollection[G[+_], T](implicit
        ev: O <:< G[T],
        G: Traverse[G]
    ): Cursor[λ[`+a` => F[G[a]]], T] =
      Success[λ[`+a` => F[G[a]]], T](value.map { case Cursor.Value(history, o) =>
        ev.apply(o).map(Cursor.Value(history, _))
      })(F.compose(G))
  }

  object Success {
    def identity[A](value: Cursor.Value[A]): Cursor.Success[Identity, A] = Success[Identity, A](value)
  }

  final case class Failure(errors: Cursor.Errors) extends Cursor[Nothing, Nothing] {
    override def modifyHistory(f: Selection.History => Selection.History): Cursor[Nothing, Nothing] =
      Failure(errors.modifyHistory(f))

    override def run: Cursor[Identity, Nothing] = this

    override def map[T](f: Nothing => T): Cursor[Nothing, T] = this

    override def andThen[T](f: Nothing => Cursor[Identity, T]): Cursor[Nothing, T] = this

    override def andThen[T](validation: CursorValidation[Nothing, T]): Cursor[Nothing, T] = this

    override def validate[T](validation: Validation[Nothing, T]): Cursor[Nothing, T] = this

    override def field[T](name: String, select: Nothing => T): Cursor[Nothing, T] = this

    override def oneOf[T](f: Nothing => (String, Validated[Errors, T])): Cursor[Nothing, T] = this

    override def unnamedOneOf[T](f: Nothing => Validated[Errors, T]): Cursor[Nothing, T] = this

    override def option[T](implicit ev: Nothing <:< Option[T]): Cursor[Nothing, T] = this

    override def collection[G[+_], T](implicit ev: Nothing <:< G[T], G: Traverse[G]): Cursor[Nothing, T] = this

    override def unindexedCollection[G[+_], T](implicit ev: Nothing <:< G[T], G: Traverse[G]): Cursor[Nothing, T] =
      this
  }

  def pure[F[+_]: Applicative: Traverse, A](value: A): Cursor[F, A] =
    Success[F, A](Cursor.Value(Selection.History.Root, value).pure[F])

  def root[A](value: A): Cursor[Identity, A] = pure[Identity, A](value)

  def withHistory[A](history: Selection.History, value: A): Cursor[Identity, A] =
    Success.identity(Cursor.Value(history, value))

  def fromValidated[A](value: Validated[Cursor.Errors, A]): Cursor[Identity, A] =
    value.fold(Failure.apply, a => Success.identity(Value(Selection.History.Root, a)))

  def fromValidatedNel[A](value: ValidatedNel[Validation.Error, A]): Cursor[Identity, A] =
    fromValidated(value.leftMap(Errors.root))

  implicit def applicative[F[+_]: Applicative: Traverse]: Applicative[Cursor[F, *]] =
    new Applicative[Cursor[F, *]] {
      override def pure[T](x: T): Cursor[F, T] = Cursor.pure(x)

      override def map[A, B](fa: Cursor[F, A])(f: A => B): Cursor[F, B] = fa.map(f)

      override def ap[A, B](ff: Cursor[F, A => B])(fa: Cursor[F, A]): Cursor[F, B] = (ff, fa) match {
        case (Success(ff), Success(fa)) =>
          Success(Applicative[F].map2(ff, fa)((ff, fa) => Value(ff.history ++ fa.history, ff.value.apply(fa.value))))
        case (Failure(ff), Failure(fa)) => Failure(ff merge fa)
        case (result: Failure, _)       => result
        case (_, result: Failure)       => result
      }
    }
}

package io.hireproof.screening.generic

import cats._
import cats.data.{Validated, ValidatedNel}
import cats.syntax.all._
import io.hireproof.screening.{Selection, Validation}

sealed abstract class Cursor[+F[+_], +A] { self =>
  def modifyHistory(f: Selection.History => Selection.History): Cursor[F, A]

  def run: Validated[Validation.Errors, F[A]]

  final def runWith[T](validation: Validation[A, T]): Validated[Validation.Errors, F[T]] = validate(validation).run

  def map[T](f: A => T): Cursor[F, T]

  def flatMap[T](f: A => Cursor[Identity, T]): Cursor[F, T]

  def andThen[T](validation: CursorValidation[A, T]): Cursor[F, T]

  final def andThen[T](f: Cursor.Root[A] => Validated[Validation.Errors, T]): Cursor[F, T] =
    andThen(CursorValidation(f))

  def validate[T](validation: Validation[A, T]): Cursor[F, T]

  final def ensure[T](validation: Validation[A, Unit]): Cursor[F, A] = validate(validation.tap)

  def oneOf[T](f: A => (String, Validated[Validation.Errors, T])): Cursor[F, T]

  def unnamedOneOf[T](f: A => Validated[Validation.Errors, T]): Cursor[F, T]

  def field[T](name: String, select: A => T): Cursor[F, T]
}

object Cursor {
  type Root[+A] = Cursor[Identity, A]

  implicit final class Ops[F[+_], A](cursor: Cursor[F, A])(implicit F: Traverse[F]) {
    def option[T](implicit ev: A <:< Option[T]): Cursor[λ[`+a` => F[Option[a]]], T] = cursor match {
      case Success(value) =>
        Success[λ[`+a` => F[Option[a]]], T](F.map(value) { case Cursor.Value(history, o) =>
          ev.apply(o).map(Cursor.Value(history, _))
        })(F.compose[Option])
      case cursor: Failure => cursor
    }

    def option[T](name: String, select: A => Option[T]): Cursor[λ[`+a` => F[Option[a]]], T] =
      cursor.field(name, select).option

    def collection[G[+_], T](implicit ev: A =:= G[T], G: Traverse[G]): Cursor[λ[`+a` => F[G[a]]], T] = cursor match {
      case Success(value) =>
        Success[λ[`+a` => F[G[a]]], T](F.map(value) { case Cursor.Value(history, o) =>
          ev.apply(o).mapWithIndex((b, index) => Cursor.Value(history / index, b))
        })(F.compose(G))
      case cursor: Failure => cursor
    }

    def collection[G[+_]: Traverse, T](name: String, select: A => G[T]): Cursor[λ[`+a` => F[G[a]]], T] =
      cursor.field(name, select).collection

    def unindexedCollection[G[+_], T](implicit ev: A =:= G[T], G: Traverse[G]): Cursor[λ[`+a` => F[G[a]]], T] =
      cursor match {
        case Success(value) =>
          Success[λ[`+a` => F[G[a]]], T](F.map(value) { case Cursor.Value(history, o) =>
            ev.apply(o).map(Cursor.Value(history, _))
          })(F.compose(G))
        case cursor: Failure => cursor
      }
  }

  final case class Value[+A](history: Selection.History, value: A) {
    def modifyHistory(f: Selection.History => Selection.History): Cursor.Value[A] = copy(history = f(history))

    def map[B](f: A => B): Cursor.Value[B] = copy(value = f(value))
  }

  final case class Success[F[+_], A](value: F[Cursor.Value[A]])(implicit F: Traverse[F]) extends Cursor[F, A] {
    override def modifyHistory(f: Selection.History => Selection.History): Cursor[F, A] =
      Success[F, A](value.map(_.modifyHistory(f)))

    override def run: Validated[Validation.Errors, F[A]] =
      F.traverse[Validated[Validation.Errors, *], Value[A], A](value) { case Cursor.Value(_, o) => Validated.valid(o) }

    override def map[T](f: A => T): Cursor[F, T] = Success(value.map(_.map(f)))

    override def flatMap[T](f: A => Cursor[Identity, T]): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, o) => f(o).modifyHistory(history ++ _) } match {
        case Success(value: Cursor.Value[F[T]]) => Success(value.value.map(Cursor.Value(value.history, _)))
        case cursor: Failure                    => cursor
      }

    override def andThen[T](validation: CursorValidation[A, T]): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, o) =>
        Cursor.fromValidated(validation.apply(Cursor.root(o))).modifyHistory(history ++ _)
      } match {
        case Success(value: Cursor.Value[F[T]]) => Success(value.value.map(Cursor.Value(value.history, _)))
        case cursor: Failure                    => cursor
      }

    override def validate[T](validation: Validation[A, T]): Cursor[F, T] = {
      F.traverse(value) { case Cursor.Value(history, a) =>
        validation
          .run(a)
          .leftMap(Validation.Errors.one(history, _))
          .map(b => Cursor.Value(history, b))
      }.fold(Cursor.Failure.apply, Cursor.Success[F, T])
    }

    override def field[T](name: String, select: A => T): Cursor[F, T] =
      Success(value.map(_.map(select))).modifyHistory(_ / name)

    override def oneOf[T](f: A => (String, Validated[Validation.Errors, T])): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, a) =>
        val (name, result) = f(a)
        result.leftMap(_.modifyHistory(history / name ++ _)).map(Cursor.Value(history / name, _))
      }.fold(Failure.apply, Success[F, T])

    override def unnamedOneOf[T](f: A => Validated[Validation.Errors, T]): Cursor[F, T] =
      F.traverse(value) { case Cursor.Value(history, a) =>
        f(a).leftMap(_.modifyHistory(history ++ _)).map(Cursor.Value(history, _))
      }.fold(Failure.apply, Success[F, T])
  }

  object Success {
    def identity[A](value: Cursor.Value[A]): Cursor.Success[Identity, A] = Success[Identity, A](value)
  }

  final case class Failure(errors: Validation.Errors) extends Cursor[Nothing, Nothing] {
    override def modifyHistory(f: Selection.History => Selection.History): Cursor[Nothing, Nothing] =
      Failure(errors.modifyHistory(f))

    override def run: Validated[Validation.Errors, Nothing] = Validated.invalid(errors)

    override def map[T](f: Nothing => T): Cursor[Nothing, T] = this

    override def flatMap[T](f: Nothing => Cursor[Identity, T]): Cursor[Nothing, T] = this

    override def andThen[T](validation: CursorValidation[Nothing, T]): Cursor[Nothing, T] = this

    override def validate[T](validation: Validation[Nothing, T]): Cursor[Nothing, T] = this

    override def field[T](name: String, select: Nothing => T): Cursor[Nothing, T] = this

    override def oneOf[T](f: Nothing => (String, Validated[Validation.Errors, T])): Cursor[Nothing, T] = this

    override def unnamedOneOf[T](f: Nothing => Validated[Validation.Errors, T]): Cursor[Nothing, T] = this
  }

  def pure[F[+_]: Applicative: Traverse, A](value: A): Cursor[F, A] =
    Success[F, A](Cursor.Value(Selection.History.Root, value).pure[F])

  def root[A](value: A): Cursor[Identity, A] = pure[Identity, A](value)

  def withHistory[A](history: Selection.History, value: A): Cursor[Identity, A] =
    Success.identity(Cursor.Value(history, value))

  def fromValidated[A](value: Validated[Validation.Errors, A]): Cursor[Identity, A] =
    value.fold(Failure.apply, a => Success.identity(Value(Selection.History.Root, a)))

  def fromValidatedNel[A](value: ValidatedNel[Validation.Error, A]): Cursor[Identity, A] =
    fromValidated(value.leftMap(Validation.Errors.root))

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

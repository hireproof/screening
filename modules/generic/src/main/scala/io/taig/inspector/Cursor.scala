package io.taig.inspector

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import cats.{Applicative, Functor, Id, Semigroup, Traverse}

import scala.collection.immutable.HashMap

abstract class Cursor[F[_], A] { self =>
  def get: Cursor.Result[F, A]

  final def run[B](validation: Validation[A, B])(implicit F: Traverse[F]): Cursor.Result[Id, F[B]] =
    andThen(validation).get.toId

  final def map[B](f: A => B)(implicit F: Functor[F]): Cursor[F, B] = new Cursor[F, B] {
    override def get: Cursor.Result[F, B] = self.get.map(f)
  }

  final def field[B](name: String, select: A => B)(implicit F: Functor[F]): Cursor[F, B] = new Cursor[F, B] {
    override def get: Cursor.Result[F, B] = self.get.map(select).modifyHistory(name :: _)
  }

  final def branch[B](validate: A => (String, CursorValidation[A, B]))(implicit F: Functor[F]): Cursor[F, B] =
    new Cursor[F, B] {
      override def get: Cursor.Result[F, B] = ???
    }

  final def option[B](implicit ev: A =:= Option[B], F: Functor[F]): Cursor[λ[a => F[Option[a]]], B] =
    new Cursor[λ[a => F[Option[a]]], B] {
      override def get: Cursor.Result[λ[a => F[Option[a]]], B] =
        self.get.map(ev.apply).mapValue[F, Option, Option[B], B] { case Cursor.Value(history, value) =>
          value.map(Cursor.Value(history, _))
        }
    }

  final def option[B](name: String, select: A => Option[B])(implicit F: Functor[F]): Cursor[λ[a => F[Option[a]]], B] =
    new Cursor[λ[a => F[Option[a]]], B] {
      override def get: Cursor.Result[λ[a => F[Option[a]]], B] = self.field(name, select).option.get
    }

  final def collection[G[_], B](implicit ev: A =:= G[B], F: Functor[F], G: Traverse[G]): Cursor[λ[a => F[G[a]]], B] =
    new Cursor[λ[a => F[G[a]]], B] {
      override def get: Cursor.Result[λ[a => F[G[a]]], B] =
        self.get.map(ev.apply).mapValue[F, G, G[B], B] { case Cursor.Value(history, value) =>
          value.zipWithIndex.map { case (b, index) => Cursor.Value(index :: history, b) }
        }
    }

  final def collection[G[_], B](name: String, select: A => G[B])(implicit
      F: Functor[F],
      G: Traverse[G]
  ): Cursor[λ[a => F[G[a]]], B] = new Cursor[λ[a => F[G[a]]], B] {
    override def get: Cursor.Result[λ[a => F[G[a]]], B] = self.field(name, select).collection.get
  }

  final def andThen[B](validation: Validation[A, B])(implicit F: Traverse[F]): Cursor[F, B] = new Cursor[F, B] {
    override def get: Cursor.Result[F, B] = self.get.andThen(validation)
  }

  final def ensure(validation: Validation[A, Unit])(implicit F: Traverse[F]): Cursor[F, A] = andThen(validation.tap)
}

object Cursor {
  final case class Value[+A](history: Selection.History, value: A) {
    def modifyHistory(f: Selection.History => Selection.History): Value[A] = copy(history = f(history))

    def map[B](f: A => B): Value[B] = copy(value = f(value))
  }

  sealed abstract class Result[+F[_], +A] {
    final def modifyHistory[FF[a] >: F[a]](
        f: Selection.History => Selection.History
    )(implicit F: Functor[FF]): Result[FF, A] = this match {
      case Success(value) => Success(F.map(value)(_.modifyHistory(f)))
      case result: Errors => result.modifyHistory(f)
    }

    final def map[FF[a] >: F[a], B](f: A => B)(implicit F: Functor[FF]): Cursor.Result[FF, B] = this match {
      case Success(value) => Success(F.map(value)(_.map(f)))
      case result: Errors => result
    }

    final def mapF[FF[a] >: F[a], G[_], B](
        f: A => G[B]
    )(implicit F: Functor[FF], G: Functor[G]): Cursor.Result[λ[a => FF[G[a]]], B] = this match {
      case Success(value) => Success[λ[a => FF[G[a]]], B](F.map(value)(a => f(a.value).map(Value(a.history, _))))
      case result: Errors => result
    }

    final def mapValue[FF[a] >: F[a], G[_], AA >: A, B](
        f: Value[AA] => G[Value[B]]
    )(implicit F: Functor[FF]): Cursor.Result[λ[a => FF[G[a]]], B] =
      this match {
        case Success(value) => Success[λ[a => FF[G[a]]], B](F.map(value)(f))
        case result: Errors => result
      }

    final def andThen[FF[a] >: F[a], B](validation: Validation[A, B])(implicit F: Traverse[FF]): Cursor.Result[FF, B] =
      this match {
        case Success(value) =>
          Result.fromValidatedValue(F.traverse(value) { case Value(history, value) =>
            validation.run(value).bimap(Cursor.Errors.one(history, _), Value(history, _))
          })
        case failure: Errors => failure
      }

    final def andThen[FF[a] >: F[a], G[_]: Applicative, B](
        f: A => Result[G, B]
    )(implicit F: Traverse[FF]): Result[G, FF[B]] =
      this match {
        case Success(value)  => F.traverse(value) { case Value(history, value) => f(value).modifyHistory(history ++ _) }
        case failure: Errors => failure
      }

    final def toValidated[FF[a] >: F[a], AA >: A](implicit F: Functor[FF]): Validated[Cursor.Errors, FF[AA]] =
      this match {
        case Success(value) => Validated.valid(F.map(value)(_.value))
        case result: Errors => Validated.invalid(result)
      }

    final def toId[FF[a] >: F[a], AA >: A](implicit F: Functor[FF]): Cursor.Result[Id, FF[AA]] = this match {
      case Success(value) => Success[Id, FF[AA]](Value(Selection.History.Root, F.map(value)(_.value)))
      case result: Errors => result
    }
  }

  object Result {
    def fromValidatedValue[F[_], A](validated: Validated[Cursor.Errors, F[Value[A]]]): Cursor.Result[F, A] =
      validated match {
        case Validated.Valid(value)     => Success(value)
        case Validated.Invalid(failure) => failure
      }

    def fromValidatedNel[F[_]: Applicative, A](validated: ValidatedNel[Validation.Error, A]): Cursor.Result[F, A] =
      validated match {
        case Validated.Valid(value)    => Success(Value(Selection.History.Root, value).pure[F])
        case Validated.Invalid(errors) => Cursor.Errors.one(Selection.History.Root, errors)
      }

    implicit def applicative[F[_]](implicit F: Applicative[F]): Applicative[Cursor.Result[F, *]] =
      new Applicative[Cursor.Result[F, *]] {
        override def pure[A](x: A): Result[F, A] = Success(Value(Selection.History.Root, x).pure[F])

        override def map[A, B](fa: Result[F, A])(f: A => B): Result[F, B] = fa.map(f)

        override def ap[A, B](ff: Result[F, A => B])(fa: Result[F, A]): Result[F, B] = (fa, ff) match {
          case (Success(fa), Success(ff)) =>
            Success((fa, ff).mapN((fa, ff) => Value(fa.history ++ ff.history, ff.value(fa.value))))
          case (fa: Errors, ff: Errors) => fa merge ff
          case (fa: Errors, _)          => fa
          case (_, ff: Errors)          => ff
        }
      }
  }

  final case class Success[F[_], A](value: F[Cursor.Value[A]]) extends Result[F, A]

  object Success {
    def root[A](value: A): Cursor.Result[Id, A] = Success[Id, A](Value(Selection.History.Root, value))
  }

  final class Errors(
      head: (Selection.History, NonEmptyList[Validation.Error]),
      tail: HashMap[Selection.History, NonEmptyList[Validation.Error]]
  ) extends Result[Nothing, Nothing] {
    def modifyHistory(f: Selection.History => Selection.History): Cursor.Errors =
      Errors.unsafeFromMap(toMap.map { case (history, errors) => (f(history), errors) })

    def merge(failure: Errors): Cursor.Errors = Errors.unsafeFromMap {
      failure.toMap.foldLeft(toMap) { case (result, (history, errors)) =>
        result.updatedWith(history) {
          case Some(current) => Some(current concatNel errors)
          case None          => Some(errors)
        }
      }
    }

    def toMap: HashMap[Selection.History, NonEmptyList[Validation.Error]] = tail + head

    override def hashCode(): Int = toMap.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case failure: Errors => toMap equals failure.toMap
      case _               => false
    }

    override def toString: String = s"Cursor.Failure($toMap)"
  }

  object Errors {
    def fromMap(values: Map[Selection.History, NonEmptyList[Validation.Error]]): Option[Cursor.Errors] =
      Option.when(values.nonEmpty)(new Errors(values.head, values.tail.to(HashMap)))

    def unsafeFromMap(values: Map[Selection.History, NonEmptyList[Validation.Error]]): Cursor.Errors =
      fromMap(values).get

    def apply(
        head: (Selection.History, NonEmptyList[Validation.Error]),
        tail: (Selection.History, NonEmptyList[Validation.Error])*
    ): Cursor.Errors = unsafeFromMap(tail.toMap + head)

    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Cursor.Errors =
      Errors(history -> errors)

    implicit val semigroup: Semigroup[Cursor.Errors] = new Semigroup[Cursor.Errors] {
      override def combine(x: Errors, y: Errors): Errors = x merge y
    }
  }

  def root[A](value: A): Cursor[Id, A] = new Cursor[Id, A] {
    override val get: Result[Id, A] = Success[Id, A](Value(Selection.History.Root, value))
  }
}

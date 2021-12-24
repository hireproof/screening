package io.taig.inspector

import cats.data.{NonEmptyList, NonEmptyMap, Validated, ValidatedNel}
import cats.implicits._
import cats.{Applicative, Functor, Id, Semigroup, Traverse}

import scala.collection.immutable.SortedMap

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

  final def oneOf[G[_]: Applicative, B](
      validate: A => (String, Cursor.Result[G, B])
  )(implicit F: Traverse[F]): Cursor.Result[Id, F[B]] = ???
//    new Cursor[G, F[B]] {
//      override def get: Cursor.Result[G, F[B]] = self.get.andThen { a =>
//        val (name, result) = validate(a)
//        result.modifyHistory(name :: _)
//      }
//    }

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

  // TODO PartialSuccess
  final def ensure(validation: Validation[A, Unit])(implicit F: Traverse[F]): Cursor[F, A] = andThen(validation.tap)
}

object Cursor {
  final case class Value[+A](history: Selection.History, value: A) {
    def modifyHistory(f: Selection.History => Selection.History): Value[A] = copy(history = f(history))

    def map[B](f: A => B): Value[B] = copy(value = f(value))
  }

  sealed abstract class Result[+F[_], +A] extends Product with Serializable {
    final def modifyHistory[FF[a] >: F[a]](
        f: Selection.History => Selection.History
    )(implicit F: Functor[FF]): Result[FF, A] = this match {
      case Success(value)                 => Success(F.map(value)(_.modifyHistory(f)))
      case PartialSuccess(failure, value) => PartialSuccess(failure.modifyHistory(f), F.map(value)(_.modifyHistory(f)))
      case result: Failure                => result.modifyHistory(f)
    }

    final def map[FF[a] >: F[a], B](f: A => B)(implicit F: Functor[FF]): Cursor.Result[FF, B] = this match {
      case Success(value)                 => Success(F.map(value)(_.map(f)))
      case PartialSuccess(failure, value) => PartialSuccess(failure, F.map(value)(_.map(f)))
      case result: Failure                => result
    }

    final def mapF[FF[a] >: F[a], G[_], B](
        f: A => G[B]
    )(implicit F: Functor[FF], G: Functor[G]): Cursor.Result[λ[a => FF[G[a]]], B] = this match {
      case Success(value) => Success[λ[a => FF[G[a]]], B](F.map(value)(a => f(a.value).map(Value(a.history, _))))
      case PartialSuccess(failure, value) =>
        PartialSuccess[λ[a => FF[G[a]]], B](failure, F.map(value)(a => f(a.value).map(Value(a.history, _))))
      case result: Failure => result
    }

    final def mapValue[FF[a] >: F[a], G[_], AA >: A, B](
        f: Value[AA] => G[Value[B]]
    )(implicit F: Functor[FF]): Cursor.Result[λ[a => FF[G[a]]], B] =
      this match {
        case Success(value)                 => Success[λ[a => FF[G[a]]], B](F.map(value)(f))
        case PartialSuccess(failure, value) => PartialSuccess[λ[a => FF[G[a]]], B](failure, F.map(value)(f))
        case result: Failure                => result
      }

    final def andThen[FF[a] >: F[a], B](validation: Validation[A, B])(implicit F: Traverse[FF]): Cursor.Result[FF, B] =
      this match {
        case Success(value) =>
          F.traverse(value) { case Value(history, value) =>
            validation.run(value).bimap(Cursor.Failure.one(history, _), Value(history, _))
          } match {
            case Validated.Valid(value)     => Success(value)
            case Validated.Invalid(failure) => failure
          }
        case PartialSuccess(failure, value) =>
          F.traverse(value) { case Value(history, value) =>
            validation.run(value).bimap(failure merge Cursor.Failure.one(history, _), Value(history, _))
          } match {
            case Validated.Valid(value)     => PartialSuccess(failure, value)
            case Validated.Invalid(failure) => failure
          }
        case failure: Failure => failure
      }

    final def andThen[FF[a] >: F[a], G[_]: Applicative, B](
        f: A => Result[G, B]
    )(implicit F: Traverse[FF]): Result[G, FF[B]] = this match {
      case Success(value) => F.traverse(value) { case Value(history, value) => f(value).modifyHistory(history ++ _) }
      case PartialSuccess(failure1, value) =>
        F.traverse(value) { case Value(history, value) =>
          f(value).modifyHistory(history ++ _) match {
            case Success(value)                  => PartialSuccess(failure1, value)
            case PartialSuccess(failure2, value) => PartialSuccess(failure1 merge failure2, value)
            case result: Failure                 => failure1 merge result
          }
        }
      case failure: Failure => failure
    }

    final def toValidated[FF[a] >: F[a], AA >: A](implicit F: Functor[FF]): Validated[Cursor.Failure, FF[AA]] =
      this match {
        case Success(value)             => Validated.valid(F.map(value)(_.value))
        case PartialSuccess(failure, _) => Validated.invalid(failure)
        case result: Failure            => Validated.invalid(result)
      }

    final def toId[FF[a] >: F[a], AA >: A](implicit F: Functor[FF]): Cursor.Result[Id, FF[AA]] = this match {
      case Success(value) => Success[Id, FF[AA]](Value(Selection.History.Root, F.map(value)(_.value)))
      case PartialSuccess(failure, value) =>
        PartialSuccess[Id, FF[AA]](failure, Value(Selection.History.Root, F.map(value)(_.value)))
      case result: Failure => result
    }
  }

  object Result {
    def fromValidatedNel[A](validated: ValidatedNel[Validation.Error, A]): Cursor.Result[Id, A] =
      validated match {
        case Validated.Valid(value)    => Success[Id, A](Value(Selection.History.Root, value))
        case Validated.Invalid(errors) => Cursor.Failure.one(Selection.History.Root, errors)
      }

    implicit def applicative[F[_]](implicit F: Applicative[F]): Applicative[Cursor.Result[F, *]] =
      new Applicative[Cursor.Result[F, *]] {
        override def pure[A](x: A): Result[F, A] = Success(Value(Selection.History.Root, x).pure[F])

        override def map[A, B](fa: Result[F, A])(f: A => B): Result[F, B] = fa.map(f)

        override def ap[A, B](ff: Result[F, A => B])(fa: Result[F, A]): Result[F, B] = (fa, ff) match {
          case (Success(fa), Success(ff)) =>
            Success((fa, ff).mapN((fa, ff) => Value(fa.history ++ ff.history, ff.value(fa.value))))
          case (fa: Failure, ff: Failure) => fa merge ff
          case (fa: Failure, _)           => fa
          case (_, ff: Failure)           => ff
        }
      }
  }

  final case class Success[F[_], A](value: F[Cursor.Value[A]]) extends Result[F, A]

  object Success {
    def root[A](value: A): Cursor.Result[Id, A] = Success[Id, A](Value(Selection.History.Root, value))
  }

  final case class PartialSuccess[F[_], A](failure: Cursor.Failure, value: F[Cursor.Value[A]]) extends Result[F, A]

  final case class Failure(toNem: NonEmptyMap[Selection.History, NonEmptyList[Validation.Error]])
      extends Result[Nothing, Nothing] {
    def modifyHistory(f: Selection.History => Selection.History): Cursor.Failure = Failure(toNem.mapKeys(f))

    def merge(failure: Failure): Failure = Failure.unsafeFromMap {
      failure.toMap.foldLeft(toMap) { case (result, (history, errors)) =>
        result.updatedWith(history) {
          case Some(current) => Some(current concatNel errors)
          case None          => Some(errors)
        }
      }
    }

    def toMap: SortedMap[Selection.History, NonEmptyList[Validation.Error]] = toNem.toSortedMap
  }

  object Failure {
    def fromMap(values: Map[Selection.History, NonEmptyList[Validation.Error]]): Option[Cursor.Failure] =
      NonEmptyMap.fromMap(SortedMap.from(values)).map(apply)

    def unsafeFromMap(values: Map[Selection.History, NonEmptyList[Validation.Error]]): Cursor.Failure =
      Failure(NonEmptyMap.fromMapUnsafe(SortedMap.from(values)))

    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Cursor.Failure =
      Failure(NonEmptyMap.one(history, errors))

    def of(
        head: (Selection.History, NonEmptyList[Validation.Error]),
        tail: (Selection.History, NonEmptyList[Validation.Error])*
    ): Cursor.Failure =
      Failure(NonEmptyMap(head, SortedMap.from(tail)))

    implicit val semigroup: Semigroup[Cursor.Failure] = new Semigroup[Cursor.Failure] {
      override def combine(x: Failure, y: Failure): Failure = x merge y
    }
  }

  def root[A](value: A): Cursor[Id, A] = new Cursor[Id, A] {
    override val get: Result[Id, A] = Success[Id, A](Value(Selection.History.Root, value))
  }
}

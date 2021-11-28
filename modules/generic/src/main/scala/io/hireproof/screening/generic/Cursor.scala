package io.hireproof.screening.generic

import cats._
import cats.data.{NonEmptyList, NonEmptyMap, Validated}
import cats.syntax.all._
import io.hireproof.screening.Validation

import scala.collection.immutable.SortedMap

abstract class Cursor[F[_], A](implicit F: Traverse[F]) { self =>
  def get: Cursor.Result[F, A]

  final def modifyHistory(f: Selection.History => Selection.History): Cursor[F, A] = Cursor(get.modifyHistory(f))

  final def run: Cursor.Result[Id, F[A]] = get match {
    case Cursor.Result.Success(value) =>
      F.traverse(value) { case Cursor.Value(_, a) =>
        Cursor.Result.Success[Id, A](Cursor.Value(Selection.History.Root, a)): Cursor.Result[Id, A]
      }
    case result: Cursor.Result.Failure => result
  }

  final def runWith[B](validation: Validation[A, B]): Cursor.Result[Id, F[B]] = validate(validation).run

  final def runOneOf[B](f: A => (String, Validated[Cursor.Errors, B])): Cursor.Result[Id, F[B]] = oneOf(f).run

  final def map[B](f: A => B): Cursor[F, B] = Cursor(get.map(f))

  final def root: Cursor[Id, F[A]] = Cursor(run)

  final def andThen[B](validation: CursorValidation[A, B]): Cursor[F, B] = Cursor {
    get match {
      case Cursor.Result.Success(value) =>
        F.traverse(value) { case Cursor.Value(history, a) =>
          validation.get(Cursor.root(a)).modifyHistory(history ++ _)
        } match {
          case Cursor.Result.Success(value: Cursor.Value[F[B]]) =>
            Cursor.Result.Success(value.value.map(Cursor.Value(value.history, _)))
          case result: Cursor.Result.Failure => result
        }
      case result: Cursor.Result.Failure => result
    }
  }

  final def oneOf[B](f: A => (String, Validated[Cursor.Errors, B])): Cursor[F, B] = Cursor {
    get match {
      case Cursor.Result.Success(value) =>
        F.traverse(value) { case Cursor.Value(history, a) =>
          val (name, result) = f(a)
          result.leftMap(_.modifyHistory(history / name ++ _)).map(Cursor.Value(history / name, _))
        }.fold(Cursor.Result.Failure.apply, Cursor.Result.Success[F, B])
      case result: Cursor.Result.Failure => result
    }
  }

  final def field[B](name: String, select: A => B): Cursor[F, B] = Cursor(get.map(select).modifyHistory(_ / name))

  final def option[B](implicit ev: A =:= Option[B]): Cursor[λ[a => F[Option[a]]], B] =
    Cursor[λ[a => F[Option[a]]], B] {
      get.mapValue[Option, B] { case Cursor.Value(history, value) =>
        ev.apply(value).map(Cursor.Value(history, _))
      }
    }(F.compose[Option])

  final def option[B](name: String, select: A => Option[B]): Cursor[λ[a => F[Option[a]]], B] =
    field(name, select).option

  final def collection[G[_], B](implicit ev: A =:= G[B], G: Traverse[G]): Cursor[λ[a => F[G[a]]], B] =
    Cursor[λ[a => F[G[a]]], B] {
      get.mapValue[G, B] { case Cursor.Value(history, value) =>
        ev.apply(value).mapWithIndex((b, index) => Cursor.Value(history / index, b))
      }
    }(F.compose(G))

  final def collection[G[_]: Traverse, B](name: String, select: A => G[B]): Cursor[λ[a => F[G[a]]], B] =
    field(name, select).collection

  final def unindexedCollection[G[_], B](implicit ev: A =:= G[B], G: Traverse[G]): Cursor[λ[a => F[G[a]]], B] =
    Cursor[λ[a => F[G[a]]], B] {
      get.mapValue[G, B] { case Cursor.Value(history, value) => ev.apply(value).map(Cursor.Value(history, _)) }
    }(F.compose(G))

  final def validate[B](validation: Validation[A, B])(implicit F: Traverse[F]): Cursor[F, B] = Cursor[F, B] {
    get match {
      case Cursor.Result.Success(value) =>
        F.traverse(value) { case Cursor.Value(history, a) =>
          validation
            .run(a)
            .leftMap(Cursor.Errors.one(history, _))
            .map(b => Cursor.Value(history, b))
        }.fold(Cursor.Result.Failure.apply, Cursor.Result.Success[F, B])
      case result: Cursor.Result.Failure => result
    }
  }

  final def ensure[B](validation: Validation[A, Unit])(implicit F: Traverse[F]): Cursor[F, A] = validate(validation.tap)
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

  sealed abstract class Result[+F[_], +A] extends Product with Serializable {
    def mapValue[G[_]: Traverse, B](f: Cursor.Value[A] => G[Cursor.Value[B]]): Result[λ[a => F[G[a]]], B]

    def modifyHistory(f: Selection.History => Selection.History): Cursor.Result[F, A]

    final def map[B](f: A => B): Result[F, B] = mapValue[Id, B](_.map(f))

    def toValidated[AA >: A]: Validated[Cursor.Errors, F[AA]]
  }

  object Result {
    final case class Success[F[_], A](value: F[Cursor.Value[A]])(implicit F: Traverse[F]) extends Result[F, A] {
      override def mapValue[G[_]: Traverse, B](f: Value[A] => G[Value[B]]): Result[λ[a => F[G[a]]], B] =
        Success[λ[a => F[G[a]]], B](value.map(f))(F.compose[G])

      override def modifyHistory(f: Selection.History => Selection.History): Result[F, A] =
        Success(value.map(_.modifyHistory(f)))

      override def toValidated[AA >: A]: Validated[Cursor.Errors, F[AA]] =
        value.traverse { case Cursor.Value(_, value) => Validated.valid[Cursor.Errors, A](value) }
    }

    object Success {
      def id[A](value: Cursor.Value[A]): Cursor.Result[Id, A] = Success[Id, A](value)
    }

    final case class Failure(errors: Cursor.Errors) extends Result[Nothing, Nothing] {
      override def mapValue[G[_]: Traverse, B](f: Value[Nothing] => G[Value[B]]): Result[Nothing, B] = this

      override def modifyHistory(f: Selection.History => Selection.History): Result[Nothing, Nothing] =
        Failure(errors.modifyHistory(f))

      override def toValidated[AA >: Nothing]: Validated[Cursor.Errors, Nothing] = Validated.invalid(errors)
    }

    def root[F[_]: Applicative: Traverse, A](value: A): Cursor.Result[F, A] =
      Success(Value(Selection.History.Root, value).pure[F])

    def id[A](value: A): Cursor.Result[Id, A] = Success[Id, A](Value(Selection.History.Root, value))

    implicit def applicative[F[_]: Applicative: Traverse]: Applicative[Result[F, *]] = new Applicative[Result[F, *]] {
      override def pure[A](x: A): Result[F, A] = Result.root[F, A](x)

      override def ap[A, B](ff: Result[F, A => B])(fa: Result[F, A]): Result[F, B] = (ff, fa) match {
        case (Success(ff), Success(fa)) =>
          Success(Applicative[F].map2(ff, fa)((ff, fa) => Value(ff.history ++ fa.history, ff.value.apply(fa.value))))
        case (Failure(ff), Failure(fa)) => Failure(ff merge fa)
        case (result: Failure, _)       => result
        case (_, result: Failure)       => result
      }
    }
  }

  def apply[F[_]: Traverse, A](result: => Result[F, A]): Cursor[F, A] = new Cursor[F, A] {
    override def get: Result[F, A] = result
  }

  def root[A](value: A): Cursor[Id, A] = Cursor[Id, A](Result.id(value))

  def withHistory[A](history: Selection.History, value: A): Cursor[Id, A] = root(value).modifyHistory(_ => history)
}

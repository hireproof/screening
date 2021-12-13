package io.taig.inspector

import cats.data.{NonEmptyList, NonEmptyMap, Validated}
import cats.syntax.all._
import cats.{Monad, Semigroup}
import io.circe._
import io.taig.inspector.ValidatingDecoder.Result
import io.taig.inspector.circe.toSelectionHistory

import scala.annotation.tailrec

trait ValidatingDecoder[A] { self =>
  def apply(cursor: HCursor): ValidatingDecoder.Result[A]

  final def decodeJson(json: Json): ValidatingDecoder.Result[A] = apply(json.hcursor)

  final def ensure[O](validation: Validation.Rule[A, O]): ValidatingDecoder[O] = new ValidatingDecoder[O] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[O] = self.apply(cursor).andThen { value =>
      validation.run(value).leftMap(ValidatingDecoder.Errors.one(toSelectionHistory(cursor.history), _))
    }
  }

  final def map[T](f: A => T): ValidatingDecoder[T] = new ValidatingDecoder[T] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[T] = self.apply(cursor).map(f)
  }

  final def flatMap[T](f: A => ValidatingDecoder[T]): ValidatingDecoder[T] = new ValidatingDecoder[T] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[T] =
      self.apply(cursor) match {
        case Validated.Valid(value)        => f(value)(cursor)
        case errors @ Validated.Invalid(_) => errors.asInstanceOf[ValidatingDecoder.Result[T]]
      }
  }

  final def or[AA >: A](decoder: ValidatingDecoder[AA]): ValidatingDecoder[AA] = new ValidatingDecoder[AA] {
    override def apply(cursor: HCursor): Result[AA] = self.apply(cursor).orElse(decoder.apply(cursor))
  }
}

object ValidatingDecoder {
  type Result[A] = Validated[ValidatingDecoder.Errors, A]

  def apply[A](implicit decoder: ValidatingDecoder[A]): ValidatingDecoder[A] = decoder

  implicit def fromDecoder[A](implicit decoder: Decoder[A]): ValidatingDecoder[A] = new ValidatingDecoder[A] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[A] =
      decoder.apply(cursor).leftMap(ValidatingDecoder.Errors.fromDecodingFailure).toValidated
  }

  def instance[A](f: HCursor => ValidatingDecoder.Result[A]): ValidatingDecoder[A] = new ValidatingDecoder[A] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[A] = f(cursor)
  }

  def pure[A](value: A): ValidatingDecoder[A] = new ValidatingDecoder[A] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[A] = Validated.valid(value)
  }

  final case class Errors(values: NonEmptyMap[Selection.History, Either[String, NonEmptyList[Validation.Error]]])
      extends AnyVal {
    def merge(errors: Errors): Errors = {
      val lookup = errors.values.toSortedMap

      val result = errors.values.keys.foldLeft(values.toSortedMap) { case (result, history) =>
        result.updatedWith(history) {
          case Some(Right(x)) =>
            lookup(history) match {
              case Right(y)          => Some(Right(x concatNel y))
              case failure @ Left(_) => Some(failure)
            }
          case failure @ Some(Left(_)) => failure
          case None                    => Some(lookup(history))
        }
      }

      Errors(NonEmptyMap.fromMapUnsafe(result))
    }
  }

  object Errors {
    def one(history: Selection.History, errors: NonEmptyList[Validation.Error]): Errors =
      Errors(NonEmptyMap.one(history, errors.asRight))

    def fromDecodingFailure(failure: DecodingFailure): Errors =
      Errors(NonEmptyMap.one(toSelectionHistory(failure.history), failure.message.asLeft))

    def root(errors: NonEmptyList[Validation.Error]): Errors = Errors(
      NonEmptyMap.one(Selection.History.Root, errors.asRight)
    )

    implicit val semigroup: Semigroup[Errors] = new Semigroup[Errors] {
      override def combine(x: Errors, y: Errors): Errors = Errors(x.values combine y.values)
    }
  }

  implicit val instances: Monad[ValidatingDecoder] = new Monad[ValidatingDecoder] {
    override def pure[A](x: A): ValidatingDecoder[A] = ValidatingDecoder.pure(x)

    override def flatMap[A, B](fa: ValidatingDecoder[A])(f: A => ValidatingDecoder[B]): ValidatingDecoder[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => ValidatingDecoder[Either[A, B]]): ValidatingDecoder[B] =
      new ValidatingDecoder[B] {
        @tailrec
        def step(cursor: HCursor, a: A): ValidatingDecoder.Result[B] = f(a)(cursor) match {
          case l @ Validated.Invalid(_)  => l.asInstanceOf[ValidatingDecoder.Result[B]]
          case Validated.Valid(Left(a))  => step(cursor, a)
          case Validated.Valid(Right(b)) => Validated.Valid(b)
        }

        override def apply(cursor: HCursor): Result[B] = step(cursor, a)
      }
  }
}

package io.taig.inspector

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import cats.{Monad, Semigroup}
import io.circe._
import io.taig.inspector.ValidatingDecoder.Result

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

trait ValidatingDecoder[A] { self =>
  def apply(cursor: HCursor): ValidatingDecoder.Result[A]

  final def tryDecode(cursor: ACursor): ValidatingDecoder.Result[A] = cursor match {
    case cursor: HCursor => apply(cursor)
    case _ =>
      Validated.invalid(ValidatingDecoder.Errors.one(cursor.history, Left("Attempt to decode value on failed cursor")))
  }

  final def decodeJson(json: Json): ValidatingDecoder.Result[A] = apply(json.hcursor)

  final def ensure[O](validation: Validation[A, O]): ValidatingDecoder[O] = new ValidatingDecoder[O] {
    override def apply(cursor: HCursor): ValidatingDecoder.Result[O] = self.apply(cursor).andThen { value =>
      validation.run(value).leftMap(ValidatingDecoder.Errors.fromErrors(cursor.history, _))
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

  final class Errors private (
      head: (List[CursorOp], Either[String, NonEmptyList[Validation.Error]]),
      tail: HashMap[List[CursorOp], Either[String, NonEmptyList[Validation.Error]]]
  ) {
    def merge(errors: Errors): Errors = {
      val lookup = errors.toMap

      val result = errors.keys.foldLeft(this.toMap) { case (result, history) =>
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

      Errors.unsafeFromMap(result)
    }

    def keys: Iterable[List[CursorOp]] = head._1 :: tail.keys.toList

    def toMap: HashMap[List[CursorOp], Either[String, NonEmptyList[Validation.Error]]] = tail + head

    override def hashCode(): Int = toMap.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case errors: Errors => toMap.equals(errors.toMap)
      case _              => false
    }

    override def toString: String = toMap.toString()
  }

  object Errors {
    def one(history: List[CursorOp], error: Either[String, NonEmptyList[Validation.Error]]): Errors =
      new Errors((history, error), HashMap.empty)

    def fromErrors(history: List[CursorOp], errors: NonEmptyList[Validation.Error]): Errors =
      one(history, errors.asRight)

    def fromDecodingFailure(failure: DecodingFailure): Errors = one(failure.history, failure.message.asLeft)

    def root(error: Either[String, NonEmptyList[Validation.Error]]): Errors = one(Nil, error)

    def fromMap(values: Map[List[CursorOp], Either[String, NonEmptyList[Validation.Error]]]): Option[Errors] =
      Option.when(values.nonEmpty)(new Errors(values.head, values.tail.to(HashMap)))

    def unsafeFromMap(values: Map[List[CursorOp], Either[String, NonEmptyList[Validation.Error]]]): Errors =
      fromMap(values).get

    def of(
        head: (List[CursorOp], Either[String, NonEmptyList[Validation.Error]]),
        tail: (List[CursorOp], Either[String, NonEmptyList[Validation.Error]])*
    ): Errors = unsafeFromMap(tail.toMap + head)

    implicit val semigroup: Semigroup[Errors] = new Semigroup[Errors] {
      override def combine(x: Errors, y: Errors): Errors = x merge y
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

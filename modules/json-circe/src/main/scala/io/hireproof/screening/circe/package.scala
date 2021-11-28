package io.hireproof.screening

import cats.data.Validated
import io.circe._

package object circe extends CirceInstances {
  implicit final class RichCursor(val cursor: ACursor) extends AnyVal {
    def validate[A](implicit decoder: ValidatingDecoder[A]): ValidatingDecoder.Result[A] = decoder.tryDecode(cursor)

    def validateWith[I, O](validation: Validation[I, O])(implicit decoder: Decoder[I]): ValidatingDecoder.Result[O] = {
      decoder.tryDecode(cursor) match {
        case Right(value) =>
          validation.run(value).leftMap(ValidatingDecoder.Errors.fromErrors(cursor.history, _))
        case Left(failure) => Validated.invalid(ValidatingDecoder.Errors.fromDecodingFailure(failure))
      }
    }
  }
}

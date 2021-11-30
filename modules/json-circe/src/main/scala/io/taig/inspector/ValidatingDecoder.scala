package io.taig.inspector

import io.circe.HCursor

trait ValidatingDecoder[A] {
  def apply(cursor: HCursor): ValidatingDecoder.Result[A]
}

object ValidatingDecoder {
  type Result[A] = Either[Validation.Errors, A]
}

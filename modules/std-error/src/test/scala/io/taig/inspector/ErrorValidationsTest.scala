package io.taig.inspector

final class ErrorValidationsTest extends ValidationsTest[Error] {
  override val validation: Validations[Error] = ErrorValidations
}

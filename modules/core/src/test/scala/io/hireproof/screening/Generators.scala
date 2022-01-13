package io.hireproof.screening

import org.scalacheck.Gen

object Generators {
  val boolean: Gen[Boolean] = Gen.oneOf(true, false)

  val validation: Gen[Validation[Int, Int]] = {
    val number = {
      val operator = {
        val equal = Gen.const(Validation.Number.Operator.Equal)
        val lessThan = boolean.map(Validation.Number.Operator.LessThan.apply)
        val greaterThan = boolean.map(Validation.Number.Operator.GreaterThan.apply)
        Gen.oneOf(equal, lessThan, greaterThan)
      }

      for {
        operator <- operator
        reference <- Gen.choose(Int.MinValue, Int.MaxValue)
      } yield Validation.Number(operator, reference, delta = 0)
    }

    number
      .flatMap { number =>
        boolean.map {
          case true  => Validation.Not(number)
          case false => number
        }
      }
      .map(_.tap)
  }
}

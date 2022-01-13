package io.hireproof.screening

import cats.Eq
import cats.laws.discipline.ArrowTests
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

final class ValidationLawsTest extends DisciplineSuite {
  implicit val arbitrary: Arbitrary[Validation[Int, Int]] = Arbitrary(Generators.validation)

  implicit def eq[I: Arbitrary, O: Eq]: Eq[Validation[I, O]] = Eq.instance { (x, y) =>
    Gen.listOfN(n = 1000, Arbitrary.arbitrary[I]).sample match {
      case Some(inputs) => inputs.forall(input => x.run(input) === y.run(input))
      case None         => false
    }
  }

  checkAll("Validation.ArrowLaws", ArrowTests[Validation].arrow[Int, Int, Int, Int, Int, Int])
}

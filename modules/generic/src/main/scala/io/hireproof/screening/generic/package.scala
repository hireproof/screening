package io.hireproof.screening

package object generic {
  type Identity[+A] = A

  val __ : Selection.History = Selection.History.Root
}

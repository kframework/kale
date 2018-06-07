package org.kframework.kale

import cats.Eq
import org.kframework.kale.equiv.CanBeEquivalent
import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.tests.TestSetup
import org.scalatest.FreeSpec

class EquivTest extends TestSetup[StandardEnvironment]() {

  import env._
  implicit def canEq = new CanBeEquivalent {
    override def apply(v1: Term, v2: Term) = false
  }

  import equiv._

  val eqv = implicitly[Eq[Term]].eqv _

  "string eqv" in {
    assert(eqv("10", "10"))
  }
  "string !eqv" in {
    assert(!eqv("10", "11"))
  }
}

package org.kframework.kale

import cats.Eq
import org.kframework.kale.standard.StandardEnvironment
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class PlayingWithCatsSpec extends FunSuite with Discipline {
  implicit val env = StandardEnvironment()

  import env._

  val x = Variable("x")
  val y = Variable("y")

  import equiv._
  implicit val canEquiv = new CanBeEquivalent {
    override def apply(v1: Term, v2: Term) = false
  }

  test("play") {
    assert(implicitly[Eq[Term]].eqv(x, x))
    assert(implicitly[Eq[Term]].neqv(x, y))
  }


  test("up-down scala objects") {
    assert(implicitly[UpDown[Int]].apply(3) === INT.Int(3))

    assert(implicitly[UpDown[List[Int]]].apply(List(1, 2, 3)) == scalaList(List(1, 2, 3)))

    assert(implicitly[UpDown[List[Int]]].unapply(scalaList(List(1, 2, 3))) == Some(List(1, 2, 3)))
  }
}

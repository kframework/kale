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

  test("play") {
    assert(implicitly[Eq[Term]].eqv(x, x))
    assert(implicitly[Eq[Term]].neqv(x, y))
  }


  test("up scala objects") {
    implicit val upInt =INT.Int

    assert(implicitly[Up[Int]].apply(3) === INT.Int(3))

    implicitly[MonoidLabeled[List]]

    import cats.implicits._

    assert(implicitly[Up[List[Int]]].apply(List(1, 2, 3)) == scalaList(List(1, 2, 3)))
  }
}

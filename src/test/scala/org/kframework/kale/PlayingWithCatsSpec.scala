package org.kframework.kale

import cats.Eq
import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.util.LabelNamed
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
    assert(implicitly[UpDown[Int]].up(3) === INT.Int(3))

    assert(implicitly[UpDown[List[Int]]].up(List(1, 2, 3)) == scalaList(List(1, 2, 3)))

    assert(implicitly[UpDown[List[Int]]].down(scalaList(List(1, 2, 3))) == Some(List(1, 2, 3)))
  }


  test("free lifting") {

    case class Foo(a: Int)
    case class Bar(a: Int, b: String)
    case class Buz(a: Int, b: Foo, c: String)

    implicit val freeFoo = free1(Foo)
    implicit val freeBar = free2(Bar)
    implicit val freeBuz = free3(Buz)

    val foo = Foo(5)

    val fooTerm: Term = Foo(5)

    val fooBack = fooTerm.down[Foo]

    val bar = Bar(7, "bar")
    val buz = Buz(9, foo, "buz")

    assert((foo: Term).down[Foo] === Some(foo))
    assert((bar: Term).down[Bar] === Some(bar))
    assert((buz: Term).down[Buz] === Some(buz))

  }
}

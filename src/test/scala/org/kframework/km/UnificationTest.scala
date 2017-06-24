package org.kframework.km

import org.scalatest.FreeSpec

class UnificationTest extends FreeSpec {

  import builtin._
  import term._
  import unification._

  val tt = Seq() // BOOL(true)

  val x = Variable("x", SortK)
  val y = Variable("y", SortK)
  val z = Variable("z", SortK)

  val p = new Constructor("p", (Seq(SortK,SortK),SortK))
  val q = new Constructor("q", (Seq(SortK,SortK),SortK))
  val p3 = new Constructor("p3", (Seq(SortK,SortK,SortK),SortK))

  val a = Application(new Constructor("a", (Seq(),SortK)), Seq())

  val pxy = Application(p, Seq(x, y))
  val pyx = Application(p, Seq(y, x))
  val qpp = Application(q, Seq(pxy, pyx))
  val qzz = Application(q, Seq(z, z))

  val pxya = Application(p3, Seq(x, y, a))
  val pyxx = Application(p3, Seq(y, x, x))

  val u0 = Unifier(Map(), tt)

  "simple" in {
    assert(unifyTerm(x, y, u0) == Unifier(Map(x -> y), tt))
    assert(unifyTerm(pxy, pyx, u0) == Unifier(Map(x -> y), tt)) // p(X,Y) = p(Y,X) // X -> Y
    assert(unifyTerm(qpp, qzz, u0) == Unifier(Map(x -> y, z -> pxy), tt)) // q(p(X,Y),p(Y,X)) = q(Z,Z) // X -> Y, Z -> p(X,Y)
    assert(unifyTerm(pxya, pyxx, u0) == Unifier(Map(x -> y, y -> a), tt)) // p(X,Y,a) = p(Y,X,X) // X -> Y, Y -> a
  }

}

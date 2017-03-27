package org.kframework.km

import org.scalatest.FreeSpec

class RewriteTest extends FreeSpec {

  import term._
  import builtin._

  val rewriter = new rewrite()
  import rewriter._

  val tt = BOOL(true)

  val x = Variable("x", SortK)
  val y = Variable("y", SortK)
  val z = Variable("z", SortK)

  val p = Constructor("p", (Seq(SortK,SortK),SortK))
  val q = Constructor("q", (Seq(SortK,SortK),SortK))

  val a = Application(Constructor("a", (Seq(),SortK)), Seq())
  val b = Application(Constructor("b", (Seq(),SortK)), Seq())

  val r1 = SimpleRewrite(a, b, tt)
  val t1 = SimplePattern(a, tt)

  "simple" in {
    assert(applyRule(r1, t1) == Seq(SimplePattern(b, tt)))
  }

}

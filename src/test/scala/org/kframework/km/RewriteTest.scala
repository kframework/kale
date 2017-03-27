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
  val c = Application(Constructor("c", (Seq(),SortK)), Seq())
  val d = Application(Constructor("d", (Seq(),SortK)), Seq())

  val r1 = SimpleRewrite(a, b, tt)
  val r2 = SimpleRewrite(b, c, tt)
  val r3 = SimpleRewrite(a, d, tt)
  val r4 = SimpleRewrite(a, c, tt)
  val t1 = SimplePattern(a, tt)

  "simple" in {
    // rule a => b
    // a => [ b ]
    assert(applyRule(r1, t1) == Seq(SimplePattern(b, tt)))

    // rule a => b
    // rule b => c
    // a =*=> [ c ]
    assert(search(Seq(r1,r2), t1) == Seq(SimplePattern(c, tt)))

    // rule a => b
    // rule b => c
    // rule a => d
    // a =*=> [ d, c ]  // smaller path first
    assert(search(Seq(r1,r2,r3), t1) == Seq(SimplePattern(d, tt), SimplePattern(c, tt)))

    // rule a => b
    // rule b => c
    // rule a => c
    // a =*=> [ c, c ]  // no merge
    assert(search(Seq(r1,r2,r4), t1) == Seq(SimplePattern(c, tt), SimplePattern(c, tt)))
  }

}

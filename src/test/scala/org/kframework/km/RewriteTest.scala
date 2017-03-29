package org.kframework.km

import org.scalatest.FreeSpec

class RewriteTest extends FreeSpec {

  import term._
  import builtin._

  val rewriter = new rewrite()
  import rewriter._

  val tt = BOOL(true)

  "simple" in {
    val x = Variable("x", SortK)
    val y = Variable("y", SortK)
    val z = Variable("z", SortK)

    val a = Application(Constructor("a", (Seq(),SortK)), Seq())
    val b = Application(Constructor("b", (Seq(),SortK)), Seq())
    val c = Application(Constructor("c", (Seq(),SortK)), Seq())
    val d = Application(Constructor("d", (Seq(),SortK)), Seq())

    val r1 = SimpleRewrite(a, b, tt)
    val r2 = SimpleRewrite(b, c, tt)
    val r3 = SimpleRewrite(a, d, tt)
    val r4 = SimpleRewrite(a, c, tt)

    val t1 = SimplePattern(a, tt)

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

  "symbolic" in {
    val x = Variable("x", SortInt)
    val y = Variable("y", SortInt)
    val z = Variable("z", SortInt)

    val p = Constructor("p", (Seq(SortInt),SortK))
    val q = Constructor("q", (Seq(SortInt),SortK))

    val px = Application(p, Seq(x))
    val qx = Application(q, Seq(x))

    val xgt0 = INT.gt(Seq(x, INT(0)))
    val xge0 = INT.ge(Seq(x, INT(0)))
    val xlt0 = INT.lt(Seq(x, INT(0)))

    val c = Application(Constructor("c", (Seq(),SortK)), Seq())
    val d = Application(Constructor("d", (Seq(),SortK)), Seq())

    val r1 = SimpleRewrite(px, qx, xgt0)
    val r2 = SimpleRewrite(qx, c, xge0)
    val r3 = SimpleRewrite(qx, d, xlt0)

    val t1 = SimplePattern(px, tt)

    // rule p(x:Int) => q(x) if x > 0
    // p(x) =*=> [ q(x) /\ x > 0 ]
    assert(search(Seq(r1), t1) == Seq(SimplePattern(qx, xgt0)))

    // rule p(x:Int) => q(x) if x > 0
    // rule q(x:Int) => c if x >= 0
    // rule q(x:Int) => d if x < 0
    // p(x) =*=> [ c /\ x>= 0 /\ x > 0 ]
    assert(search(Seq(r1,r2,r3), t1) == Seq(SimplePattern(c, BOOL.and(Seq(xgt0, xge0)))))
  }

  "z3" in {
    val a = Application(Constructor("a", (Seq(),SortK)), Seq())
    val b = Application(Constructor("b", (Seq(),SortK)), Seq())

    assert(z3.sat(BOOL(true)))
    assert(!z3.sat(BOOL(false)))
//    assert(try { z3.sat("(check-sat"); false } catch { case z3.Fail(msg) => msg  == "(error \"line 1 column 2: invalid command, symbol expected\")" })
    assert(!z3.sat(EQ.of(SortK)(Seq(a,b))))
  }

}

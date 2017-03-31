package org.kframework.km

import org.scalatest.FreeSpec

class RewriteTest extends FreeSpec {

  import term._
  import builtin._

  "simple" in {
    val tt = BOOL(true)

    val x = Variable("x", SortK)
    val y = Variable("y", SortK)
    val z = Variable("z", SortK)

    val al = new Constructor("a", (Seq(),SortK))
    val bl = new Constructor("b", (Seq(),SortK))
    val cl = new Constructor("c", (Seq(),SortK))
    val dl = new Constructor("d", (Seq(),SortK))

    val a = Application(al, Seq())
    val b = Application(bl, Seq())
    val c = Application(cl, Seq())
    val d = Application(dl, Seq())

    val r1 = SimpleRewrite(a, b, tt)
    val r2 = SimpleRewrite(b, c, tt)
    val r3 = SimpleRewrite(a, d, tt)
    val r4 = SimpleRewrite(a, c, tt)

    val t1 = SimplePattern(a, tt)

    val rewriter = new rewrite(z3.declareDatatypes(Seq(al,bl,cl,dl)))
    import rewriter._

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
    val tt = BOOL(true)

    val x = Variable("x", SortInt)
    val y = Variable("y", SortInt)
    val z = Variable("z", SortInt)

    val p = new Constructor("p", (Seq(SortInt),SortK))
    val q = new Constructor("q", (Seq(SortInt),SortK))

    val px = Application(p, Seq(x))
    val qx = Application(q, Seq(x))

    val xgt0 = INT.gt(x, INT(0))
    val xge0 = INT.ge(x, INT(0))
    val xlt0 = INT.lt(x, INT(0))

    val cl = new Constructor("c", (Seq(),SortK))
    val dl = new Constructor("d", (Seq(),SortK))

    val c = Application(cl, Seq())
    val d = Application(dl, Seq())

    val r1 = SimpleRewrite(px, qx, xgt0)
    val r2 = SimpleRewrite(qx, c, xge0)
    val r3 = SimpleRewrite(qx, d, xlt0)

    val t1 = SimplePattern(px, tt)

    val rewriter = new rewrite(z3.declareDatatypes(Seq(p,q,cl,dl)))
    import rewriter._

    // rule p(x:Int) => q(x) if x > 0
    // p(x) =*=> [ q(x) /\ x > 0 ]
    assert(search(Seq(r1), t1) == Seq(SimplePattern(qx, xgt0)))

    // rule p(x:Int) => q(x) if x > 0
    // rule q(x:Int) => c if x >= 0
    // rule q(x:Int) => d if x < 0
    // p(x) =*=> [ c /\ x>= 0 /\ x > 0 ]
    assert(search(Seq(r1,r2,r3), t1) == Seq(SimplePattern(c, BOOL.and(xgt0, xge0))))
  }

  "z3" in {
    val a = new Constructor("a", (Seq(),SortK))
    val b = new Constructor("b", (Seq(),SortK))
    val decl = z3.declareDatatypes(Seq(a,b))

    val aa = Application(a, Seq())
    val bb = Application(b, Seq())

    assert(z3.sat("",Set())(BOOL(true)))
    assert(!z3.sat("",Set())(BOOL(false)))
//    assert(try { z3.sat("(check-sat"); false } catch { case z3.Fail(msg) => msg  == "(error \"line 1 column 2: invalid command, symbol expected\")" })
    assert(!z3.sat(decl,Set())(EQ.of(SortK)(aa,bb)))
  }

  "0.imp" in {
    import Imp._
    val x = IdOf(STRING("x"))
    val x0 = KStmt(StmtAssign(x, AExpInt(INT(0)))) // x = 0;
    val kcell = k(kCons(x0, kNil()))
    val scell = state(M)
    val tcell = T(kcell,scell)
    val rewriter = new rewrite(declareDatatypes)
    import rewriter._
    val res = search(rules, SimplePattern(tcell, BOOL(true)))
    assert(res.toString == "List(<T>(<k>(.K()),<state>(storeMapIdInt(M:Map{Id,Int},_(STRING(x)),INT(0)))) /\\ BOOL(true))")
  }

  "1.imp" in {
    import Imp._
    val x = IdOf(STRING("x"))
    val y = IdOf(STRING("y"))
    val x0 = KStmt(StmtAssign(x, AExpInt(INT(0)))) // x = 0;
    val yx1 = KStmt(StmtAssign(y, AExpPlus(AExpId(x), AExpInt(INT(1))))) // y = x + 1;
    val kcell = k(kCons(x0, kCons(yx1, kNil())))
    val scell = state(M)
    val tcell = T(kcell,scell)
    val rewriter = new rewrite(declareDatatypes)
    import rewriter._
    val res = search(rules, SimplePattern(tcell, BOOL(true)))
    assert(res.toString == "List(<T>(<k>(.K()),<state>(storeMapIdInt(storeMapIdInt(M:Map{Id,Int},_(STRING(x)),INT(0)),_(STRING(y)),INT(1)))) /\\ BOOL(true))")
  }

  "2.imp" in {
    import Imp._
    val x = IdOf(STRING("x"))
    val y = IdOf(STRING("y"))
    val y0 = StmtAssign(y, AExpInt(INT(0))) // y = 0;
    val y1 = StmtAssign(y, AExpInt(INT(1))) // y = 1;
    val ifx0 = KStmt(StmtIf(BExpLeq(AExpId(x), AExpInt(INT(0))), y0, y1)) // if(x <= 0) { y = 0; } else { y = 1; }
    val N = Variable("N", SortInt)
    val kcell = k(kCons(ifx0, kNil()))
 // val scell = state(M) // TODO: why just state(m) doesn't work?
    val scell = state(MAP.storeOf(SortMapIdInt)(M, x, N)) // <state> M[x <- N] </state>
    val tcell = T(kcell,scell)
    val rewriter = new rewrite(declareDatatypes)
    rewriter.datatypes = datatypes
    import rewriter._
    val res = search(rules, SimplePattern(tcell, BOOL(true)))
    assert(res.toString == "List(<T>(<k>(.K()),<state>(storeMapIdInt(storeMapIdInt(M:Map{Id,Int},_(STRING(x)),N:Int),_(STRING(y)),INT(0)))) /\\ _==Bool_(BOOL(true),_<=Int_(N:Int,INT(0))), <T>(<k>(.K()),<state>(storeMapIdInt(storeMapIdInt(M:Map{Id,Int},_(STRING(x)),N:Int),_(STRING(y)),INT(1)))) /\\ _==Bool_(BOOL(false),_<=Int_(N:Int,INT(0))))")
    // <T>(<k>(.K()),<state>(storeMapIdInt(storeMapIdInt(M:Map{Id,Int},_(STRING(x)),N:Int),_(STRING(y)),INT(0)))) /\ _==Bool_(BOOL(true),_<=Int_(N:Int,INT(0)))
    // <T>(<k>(.K()),<state>(storeMapIdInt(storeMapIdInt(M:Map{Id,Int},_(STRING(x)),N:Int),_(STRING(y)),INT(1)))) /\ _==Bool_(BOOL(false),_<=Int_(N:Int,INT(0)))
  }

}

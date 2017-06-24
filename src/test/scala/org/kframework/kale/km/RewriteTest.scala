package org.kframework.kale.km

import org.kframework.kale.standard._
import org.kframework.kale.{Rewriter, SubstitutionApply, Z3Builtin}
import org.scalatest.FreeSpec

class RewriteTest extends FreeSpec {

  implicit val env = new StandardEnvironment with MultisortedMixing
  import env._

  // sort delcarations
  object Sorts {
    val Id = Sort("Id")
    val Int = new Sort("Int") with Z3Builtin
    val K = Sort("K")
  }
  import Sorts._

  // sortify builtin symbols
  sorted(ID.Id, Id)
  sorted(INT.Int, Int)

  // symbol declarations
  val a = SimpleFreeLabel0("a"); sorted(a, K)
  val b = SimpleFreeLabel0("b"); sorted(b, K)
  val c = SimpleFreeLabel0("c"); sorted(c, K)
  val d = SimpleFreeLabel0("d"); sorted(d, K)

  val p = SimpleFreeLabel1("p"); sorted(p, Int, K)
  val q = SimpleFreeLabel1("q"); sorted(q, Int, K)

  val f = SimpleFreeLabel1("f"); sorted(f, Int, Int)

  env.seal()

  val rewriter = Rewriter(new SubstitutionApply(_), env.unifier)

  "simple" in {

    val r1 = Rewrite(a(), b())
    val r2 = Rewrite(b(), c())
    val r3 = Rewrite(a(), d())
    val r4 = Rewrite(a(), c())

    val t1 = a()

    // rule a => b
    // a => [ b ]
    assert(rewriter(Set(r1)).searchStep(t1) == b())

    // rule b => c
    // a =*=> [ ]
    assert(rewriter(Set(r2)).searchStep(t1) == Bottom)

    // rule a => b
    // rule b => c
    // a => [ c ]
    val rr = rewriter(Set(r1,r2))
    assert(rr.searchStep(rr.searchStep(t1)) == c())

  }

  "symbolic" ignore {

    // variable declarations
    val X = Variable("X", Int)
    val Y = Variable("Y", Int)
    val Z = Variable("Z", Int)

    val r1 = Rewrite(
      And(Seq(p(X), Equality(INT.gt(X,INT.Int(0)), BOOLEAN.Boolean(true)))), // p(x) /\ x > 0
      q(X)
    )
    val r2 = Rewrite(
      And(Seq(q(X), Equality(INT.ge(X,INT.Int(0)), BOOLEAN.Boolean(true)))), // q(x) /\ x >= 0
      c()
    )
    val r3 = Rewrite(
      And(Seq(q(X), Equality(INT.lt(X,INT.Int(0)), BOOLEAN.Boolean(true)))), // q(x) /\ x < 0
      d()
    )

    val t1 = p(X)

    // rule p(x:Int) => q(x) if x > 0
    // p(x) =*=> [ q(x) /\ x > 0 ]
    assert(
      rewriter(Set(r1)).searchStep(t1)
        ==
      And(Seq(q(X), Equality(INT.gt(X,INT.Int(0)), BOOLEAN.Boolean(true))))
    )

    // rule p(x:Int) => q(x) if x > 0
    // rule q(x:Int) => c if x >= 0
    // rule q(x:Int) => d if x < 0
    // p(x) =*=> [ c /\ x>= 0 /\ x > 0 ]
    val rr = rewriter(Set(r1,r2,r3))
    assert(
      rr.searchStep(rr.searchStep(t1))
        ==
      And(Seq(c(), Equality(INT.ge(X,INT.Int(0)), BOOLEAN.Boolean(true)), Equality(INT.gt(X,INT.Int(0)), BOOLEAN.Boolean(true))))
    )

  }

}

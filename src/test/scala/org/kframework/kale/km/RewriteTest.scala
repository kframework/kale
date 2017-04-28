package org.kframework.kale.km

import org.kframework.kale.{And, Rewriter, SubstitutionApply}
import org.kframework.kale.standard._
import org.scalatest.FreeSpec

class RewriteTest extends FreeSpec {

  implicit val env = new KMEnvironment()
  import env._

  // sort delcarations
  object Sorts {
    val Id = Sort("Id")
    val Int = Sort("Int")
    val K = Sort("K")
  }
  import Sorts._

  // sortify builtin symbols
  sorted(ID, Id)
  sorted(INT, Int)

  // symbol declarations
  val a = FreeLabel0("a"); sorted(a, K)
  val b = FreeLabel0("b"); sorted(b, K)
  val c = FreeLabel0("c"); sorted(c, K)
  val d = FreeLabel0("d"); sorted(d, K)

  val p = FreeLabel1("p"); sorted(p, Int, K)
  val q = FreeLabel1("q"); sorted(q, Int, K)

  val f = FreeLabel1("f"); sorted(f, Int, Int)

  env.seal()

  val unifier = new MultiSortedUnifier(env)

  val rewriter = Rewriter(new SubstitutionApply(_), unifier, env)(_)

  "simple" in {

    val r1 = Rewrite(a(), b())
    val r2 = Rewrite(b(), c())
    val r3 = Rewrite(a(), d())
    val r4 = Rewrite(a(), c())

    val t1 = a()

    // rule a => b
    // a => [ b ]
    println(rewriter(Set(r1)).searchStep(t1))

    // rule b => c
    // a =*=> [ ]
    println(rewriter(Set(r2)).searchStep(t1))

  }

  "symbolic" in {
    // variable declarations
    val X = Variable("X", Int)
    val Y = Variable("Y", Int)
    val Z = Variable("Z", Int)

    val l = And(Seq(p(X), Equality(f(X), INT(0)), Equality(f(X), INT(1))))
    val r = q(X)
    val r1 = Rewrite(l, r)

    val t1 = p(X)

    val And.substitutionAndTerms(sub1, terms1) = l
    val xx = l.asInstanceOf[And].nonFormula
    val yy = l.asInstanceOf[And].formulas

    val ll = And(terms1)
    val xxx = ll.asInstanceOf[And].nonFormula
    val yyy = ll.asInstanceOf[And].formulas


    println(rewriter(Set(r1)).searchStep(t1))


  }


}

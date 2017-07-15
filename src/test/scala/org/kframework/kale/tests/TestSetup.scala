package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.context.pattern.PatternContextApplicationLabel
import org.kframework.kale.standard.{SimpleRewrite => _, _}
import org.kframework.kale.util.dsl
import org.scalactic.Prettifier
import org.scalatest.FreeSpec

abstract class TestSetup[E <: StandardEnvironment](implicit val env: E = StandardEnvironment()) extends FreeSpec {

  import env._

  val implicits = new dsl()

  val X = Variable("X")
  val Y = Variable("Y")
  val Z = Variable("Z")

  val plus = env.uniqueLabels.getOrElse("+", FreeLabel2("+")).asInstanceOf[Label2]

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = plus(x, y)
  }

  val emptyList = FreeLabel0("emptyList")

  val el = emptyList()

  val listLabel = AssocWithIdLabel("listLabel", el)

  implicit class WithListConcat(t: Term) {
    def ~~(o: Term): Term = listLabel(t, o)
  }

  val foo = FreeLabel2("foo")
  val bar = FreeLabel1("bar")
  val buz = FreeLabel2("buz")
  val List(a, b, c, d, e) = List("a", "b", "c", "d", "e").map(STRING.String)
  val List(u, v) = List("u", "v").map(STRING.String)
  val matched = FreeLabel1("matched")
  val traversed = FreeLabel1("traversed")
  val andMatchingY = FreeLabel0("andMatchingY")

  val a2b = standard.FunctionDefinedByRewritingLabel1("a2b")

  val a2bRules = Set[Rewrite](Rewrite(a2b(a), b))

  val C = Variable("C")
  val C1 = Variable("C1")

  val CAPP = PatternContextApplicationLabel("CAPP")

  CAPP.setPatterns(Or(List(
    Equality(CAPP(C, Hole), Hole),
    Equality(CAPP(C, Hole), foo(Variable("_"), CAPP(C1, Hole))),
    Equality(CAPP(C, Hole), bar(CAPP(C1, Hole)))
  )))

  env.seal()

  a2b.setRules(Or(a2bRules))

  def unifier(t1: Term, t2: Term): Term = env.And.onlyPredicate(matcher(t1, t2))

  val substitutionApplier = SubstitutionWithContext(_)

  val X_1 = Context.hole(X)

  def toAssert(t: Term): String = t match {
    case Variable((name, _)) => name.str
    case t: Node => t.toString
  }

  implicit val pretty = new Prettifier {
    override def apply(o: Any) = o match {
      case n: Node => n.toString
      case o => Prettifier.default(o)
    }
  }

  def assertRewrite(rule: Term)(obj: Term, expected: Term) {
    val actual = rule.rewrite(obj)
    assert(actual === expected)
  }
}

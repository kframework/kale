package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.context.pattern.PatternContextApplicationLabel
import org.kframework.kale.standard.{SimpleRewrite => _, _}
import org.kframework.kale.util.dsl
import org.scalactic.Prettifier

trait TestSetup {

  implicit val env = StandardEnvironment()

  import env._

  val implicits = new dsl()

  val X = Variable("X")
  val Y = Variable("Y")

  val emptyList = SimpleFreeLabel0("emptyList")

  val el = emptyList()

  val listLabel = new standard.AssocWithIdListLabel("listLabel", el)

  implicit class WithListConcat(t: Term) {
    def ~~(o: Term): Term = listLabel(t, o)
  }

  val foo = SimpleFreeLabel2("foo")
  val bar = SimpleFreeLabel1("bar")
  val buz = SimpleFreeLabel2("buz")
  val (a, b, c, d, e) = (STRING.String("a"), STRING.String("b"), STRING.String("c"), STRING.String("d"), STRING.String("e"))
  val matched = SimpleFreeLabel1("matched")
  val traversed = SimpleFreeLabel1("traversed")
  val andMatchingY = SimpleFreeLabel0("andMatchingY")

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

  implicit val rewriterBuilder: (collection.Set[_ <: Rewrite]) => Rewriter = Rewriter(SubstitutionWithContext(_)(env), SingleSortedMatcher()(env))(_)

  a2b.setRules(Or(a2bRules))

  implicit val nextUnifier = SingleSortedMatcher()

  def unifier(t1: Term, t2: Term): Term = env.And.filterOutNext(nextUnifier(t1, t2))

  val substitutionApplier = SubstitutionWithContext(_)

  val X_1 = AnywhereContext.hole(X)

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
}

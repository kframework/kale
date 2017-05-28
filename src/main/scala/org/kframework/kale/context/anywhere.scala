package org.kframework.kale.context

import org.kframework.kale._
import org.kframework.kale.standard.{Name, StandardEnvironment, SubstitutionWithContext}
import org.kframework.kale.transformer.{Binary, Unary}
import org.kframework.kale.transformer.Binary.TypedWith
import org.kframework.kale.util.Named

object anywhere {

  case class AnywhereContextApplication(label: Context1ApplicationLabel, contextVar: Variable, redex: Term) extends Node2 with Context {
    val _1: Variable = contextVar
    val _2: Term = redex
    val hole: ContextContentVariable = label.hole(contextVar)
    override lazy val isGround = false
  }


  case class AnywhereContextApplicationLabel(implicit override val env: Environment) extends Named("AnywhereContext") with Context1ApplicationLabel {
    override def apply(_1: Term, _2: Term): AnywhereContextApplication = _1 match {
      case v: Variable => AnywhereContextApplication(this, v, _2)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + _1)
    }

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }


  class AnywhereContextMatcher(implicit env: StandardEnvironment) extends transformer.Binary.ProcessingFunction[Binary.Apply] with TypedWith[AnywhereContextApplication, Term] {

    import env._

    override def f(solver: Binary.Apply)(contextApplication: AnywhereContextApplication, term: Term): Term = {
      assert(contextApplication.label == AnywhereContext)
      val contextVar = contextApplication.contextVar

      def solutionFor(subterms: Seq[Term], reconstruct: (Int, Term) => Term, avoidIndices: Set[Int] = Set()) = {
        Or((subterms.indices.toSet &~ avoidIndices) map { i =>
          // calling f directly instead of solver because we know contextApplication is hooked to the current f
          val solutionForSubtermI = f(solver)(contextApplication, subterms(i))
          val res = Or.asSet(solutionForSubtermI) map {
            // this rewires C -> HOLE into C -> foo(HOLE)
            case And.withNext(And.substitution(m), Some(Next(next))) if m.contains(contextVar) =>
              And.withNext(And.substitution(m.updated(contextVar, reconstruct(i, m(contextVar)))),
                Next(reconstruct(i, next)))
          }
          Or(res)
        })
      }

      term.label match {
        case AnywhereContext =>
          val (rightContextVar, rightContextRedex) = AnywhereContext.unapply(term).get
          solutionFor(term.children.toSeq, (_: Int, tt: Term) => AnywhereContext(rightContextVar, tt), Set(0))
        case `Or` => {
          Or(Or.asSet(term) map (solver(contextApplication, _)))
        }
        case `And` => {
          ???
        }
        case l: AssocLabel =>
          val zeroLevel: Term = And(solver(contextApplication.redex, term), Equality(contextApplication.contextVar, contextApplication.hole))
          val subresults = l.asIterable(term).toList
          val recursive = solutionFor(subresults, (pos: Int, tt: Term) => l(subresults.updated(pos, tt)))
          Or(recursive, zeroLevel)
        case l =>
          // C[bar(X)] := foo(bar(1))

          val zeroLevel: Term = And(
            // zero level tries to match bar(X) with foo(bar(X))
            solver(contextApplication.redex, term),
            // C -> HOLE
            Equality(contextApplication.contextVar, contextApplication.hole))
          val subterms = term.children
          val recursive = solutionFor(subterms.toSeq, (pos: Int, tt: Term) => term.updateAt(pos)(tt))
          Or(recursive, zeroLevel)
      }
    }
  }

  class AnywhereContextProcessingFunction(implicit env: StandardEnvironment) extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = AnywhereContextApplication

    import env._

    override def f(solver: SubstitutionApply)(t: AnywhereContextApplication): Term = {
      val recursiveResult = Equality.binding(t.hole, solver(t.redex))
      And(solver.substitution, recursiveResult) match {
        case And.withNext(subs: Substitution, _) =>
          val innerSolver = new SubstitutionWithContext(subs)(env)

          solver.substitution.get(t.contextVar) map innerSolver getOrElse AnywhereContext(t.contextVar, solver(t.redex))
        case `Bottom` => Bottom
        case _ => t // TODO: risky case; look into this at some point
      }
    }
  }

  case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
    //  assert(!basedOn.isInstanceOf[ContextContentVariable])
    val label: VariableLabel = basedOn.label

    override val name = Name(basedOn.name.str + "☐" + index)
  }

}

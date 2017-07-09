package org.kframework.kale.context

import org.kframework.kale._
import org.kframework.kale.context.anywhere.ContextContentVariable
import org.kframework.kale.context.pattern.{PatternContextApplicationLabel, PatternContextMatcher}
import org.kframework.kale.standard.{HolesMixin, Name, SubstitutionWithContext}
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunctions}
import org.kframework.kale.transformer.{Binary, Unary}
import org.kframework.kale.util.Named

trait AnywhereContextMixin extends Environment with standard.MatchingLogicMixin with HasMatcher {
  val AnywhereContext = new AnywhereLabel()

  def ANYWHERE(t: Term) = AnywhereContext(Variable.freshVariable(), t, Top)

  class AnywhereLabel()(implicit override val env: Environment) extends Named("AnywhereContext") with Label3 {
    override def apply(variable: Term, redex: Term, contextPredicate: Term = Top): AnywhereContextApplication = variable match {
      case v: Variable => AnywhereContextApplication(this, v, redex, contextPredicate)
      case env.ForAll(v: Variable, _) => AnywhereContextApplication(this, v, redex, contextPredicate)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + variable)
    }

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }

  case class AnywhereContextApplication(label: AnywhereLabel, contextVar: Variable, redex: Term, contextPredicate: Term) extends Node3 with Context {
    val _1: Variable = contextVar
    val _2: Term = redex
    val _3: Term = contextPredicate
    val hole: ContextContentVariable = label.hole(contextVar)
    override lazy val isGround = false
  }

  def AnywhereContextMatcher(solver: Apply): (AnywhereContextApplication, Term) => Term = { (contextApplication: AnywhereContextApplication, term: Term) =>

    assert(contextApplication.label == AnywhereContext)
    val contextVar = contextApplication.contextVar

    def solutionFor(subterms: Seq[Term], reconstruct: (Int, Term) => Term, avoidIndices: Set[Int] = Set()) = {
      Or((subterms.indices.toSet &~ avoidIndices) map { i: Int =>
        // calling f directly instead of solver because we know contextApplication is hooked to the current f
        val solutionForSubtermI = solver(contextApplication, subterms(i))
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
      case ac: AnywhereLabel =>
        val (rightContextVar, rightContextRedex, rightContextPredicate) = ac.unapply(term).get
        solutionFor(term.children.toSeq, (_: Int, tt: Term) => ac(rightContextVar, tt, rightContextPredicate), Set(0, 2))
      case `Or` => {
        term.asOr map (solver(contextApplication, _))
      }
      case `And` => {
        ???
      }
      case other =>
        val zeroLevel: Term = And(
          // zero level tries to match bar(X) with foo(bar(X))
          solver(contextApplication.redex, term),
          // C -> HOLE
          Equality(contextApplication.contextVar, contextApplication.hole))

        val matchPredicate = And.filterOutNext(unify(term, contextApplication.contextPredicate))
        val recursive = matchPredicate.asOr map { mp =>
          val theInnerMatch = other match {
            case l: AssocLabel =>
              val subresults = l.asIterable(term).toList
              val recursive = solutionFor(subresults, (pos: Int, tt: Term) => l(subresults.updated(pos, tt)))
              recursive
            case l =>
              // C[bar(X)] := foo(bar(1))
              val subterms = term.children
              val recursive = solutionFor(subterms.toSeq, (pos: Int, tt: Term) => term.updateAt(pos)(tt))
              recursive
          }
          theInnerMatch
        }
        Or(recursive, zeroLevel)
    }
  }
}

// TODO: un-bundle after we have decoupled the unary functions (substitution)
trait BundledContextMixin extends HolesMixin with AnywhereContextMixin with PatternContextMixin {

  class AnywhereContextProcessingFunction(implicit env: Environment with BundledContextMixin) extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = AnywhereContextApplication

    override def f(solver: SubstitutionApply)(t: AnywhereContextApplication): Term = {
      val recursiveResult = Equality.binding(t.hole, solver(t.redex))
      And(solver.substitution, recursiveResult) match {
        case And.withNext(subs: Substitution, _) =>
          val innerSolver = new SubstitutionWithContext(subs)(env)

          solver.substitution.get(t.contextVar) map innerSolver getOrElse AnywhereContext(t.contextVar, solver(t.redex), t.contextPredicate)
        case `Bottom` => Bottom
        case _ => t // TODO: risky case; look into this at some point
      }
    }
  }

  override protected def makeMatcher: ProcessingFunctions = Binary.definePartialFunction({
    case (capp: PatternContextApplicationLabel, _) => PatternContextMatcher
    case (`AnywhereContext`, _) => AnywhereContextMatcher
  }).orElse(super.makeMatcher)
}

object anywhere {


  case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
    //  assert(!basedOn.isInstanceOf[ContextContentVariable])
    val label: VariableLabel = basedOn.label

    override val name = Name(basedOn.name.str + "☐" + index)
  }

}

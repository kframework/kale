package org.kframework.kale.context

import org.kframework.kale._
import org.kframework.kale.context.anywhere.ContextContentVariable
import org.kframework.kale.context.pattern.{PatternContextApplication, PatternContextApplicationLabel}
import org.kframework.kale.standard.{HolesMixin, Name}
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunctions}
import org.kframework.kale.transformer.{Binary, Unary}
import org.kframework.kale.util.Named

trait AnywhereContextMixin extends Environment with standard.MatchingLogicMixin with HasMatcher {
  val AnywhereContext = new Named("AnywhereContext") with Label3 {
    override def apply(variable: Term, redex: Term, contextPredicate: Term = Or(anywhereTag, Variable.freshVariable())): AnywhereContextApplication = variable match {
      case v: Variable => AnywhereContextApplication(v, redex, contextPredicate)
      case env.ForAll(v: Variable, _) => AnywhereContextApplication(v, redex, contextPredicate)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + variable)
    }

    val hole = Variable("CONTEXT_HOLE")

    val anywhereTag = (new Named("anywhereMatch") with Label0 {
      override def apply(): Term = FreeNode0(this)
    }) ()

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }

  def ANYWHERE(t: Term) = AnywhereContext(Variable.freshVariable(), t, AnywhereContext.anywhereTag)

  case class AnywhereContextApplication(contextVar: Variable, redex: Term, contextPredicate: Term) extends Node3 with Context {

    val label = AnywhereContext

    val _1: Variable = contextVar
    val _2: Term = redex
    val _3: Term = contextPredicate
    val hole: ContextContentVariable = label.hole(contextVar)

    private val unfoldedContextPredicate = contextPredicate.mapBU({
      case AnywhereContext.hole => this;
      case o => o
    })

    val finalContextPredicate = unfoldedContextPredicate.variables.filter(_ != AnywhereContext.hole).foldLeft(unfoldedContextPredicate) {
      case (t, v) => Exists(v, t)
    }

    override lazy val isGround = false
  }

  def AnywhereTagMatcher(solver: Apply): (Term, Term) => Term = { (anywhereTag: Term, term: Term) =>
    assert(anywhereTag == AnywhereContext.anywhereTag)
    Next(anywhereTag)
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
          case And.SPO(And.substitution(m), p, Next(next)) =>
            And(p, Next(reconstruct(i, next)))
        }
        Or(res)
      })
    }

    term.label match {
      case AnywhereContext =>
        val (rightContextVar, rightContextRedex, rightContextPredicate) = AnywhereContext.unapply(term).get
        solutionFor(term.children.toSeq, (_: Int, tt: Term) => AnywhereContext(rightContextVar, tt, rightContextPredicate), Set(0, 2))
      case `Or` => {
        term.asOr map (solver(contextApplication, _))
      }
      case `And` => {
        ???
      }
      case other =>
        val matchPredicate = unify(contextApplication.finalContextPredicate, term)

        val res = matchPredicate.asOr map {
          case And.SPO(_, _, Next(AnywhereContext.anywhereTag)) =>
            val theAnywhereMatch = other match {
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
            theAnywhereMatch
          case And.SPO(s, p, Next(n)) if p.findBU({ case Exists(AnywhereContext.hole, _) => true; case _ => false }).isEmpty =>
            val redexSol = solver(contextApplication.redex, n)
            redexSol match {
              case And.SPO(ss, pp, Next(redexTerm)) =>
                And.SPO(ss, And(pp, Exists(AnywhereContext.hole, redexTerm)), Next(AnywhereContext.hole))
              case Bottom => Bottom
            }
          case other => other
        }

        res
    }
  }
}

// TODO: un-bundle after we have decoupled the unary functions (substitution)
trait BundledContextMixin extends HolesMixin with AnywhereContextMixin with PatternContextMixin {

  object AnywhereContextProcessingFunction extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = AnywhereContextApplication

    override def f(solver: SubstitutionApply)(t: AnywhereContextApplication): Term = {
      val recursiveResult = Equality.binding(t.hole, solver(t.redex))
      And(solver.substitution, recursiveResult) match {
        case And.withNext(subs: Substitution, _) =>
          val innerSolver = new SubstitutionWithContext(subs)

          solver.substitution.get(t.contextVar) map innerSolver getOrElse AnywhereContext(t.contextVar, solver(t.redex), t.contextPredicate)
        case `Bottom` => Bottom
        case _ => t // TODO: risky case; look into this at some point
      }
    }
  }

  case class SubstitutionWithContext(override val substitution: Substitution) extends SubstitutionApply(substitution)(env) {
    override def processingFunctions: ProcessingFunctions = definePartialFunction[Term, this.type]({
      case AnywhereContext => AnywhereContextProcessingFunction
      case l: PatternContextApplicationLabel => PatternContextProcessingFunction
    }) orElse super.processingFunctions
  }

  object PatternContextProcessingFunction extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = PatternContextApplication

    override def f(solver: SubstitutionApply)(t: PatternContextApplication): Term = {
      val contextVar = t.contextVar
      solver.substitution.get(contextVar).map({ context =>
        solver(new standard.Binding(Hole, t.redex)(env)(context))
      }).getOrElse(t.label(contextVar, solver(t.redex)))
    }
  }


  override protected def makeMatcher: ProcessingFunctions = Binary.definePartialFunction({
    case (capp: PatternContextApplicationLabel, _) => pattern.PatternContextMatcher
    case (`AnywhereContext`, _) => AnywhereContextMatcher
    case (AnywhereContext.anywhereTag.label, _) => AnywhereTagMatcher
  }).orElse(super.makeMatcher)
}

object anywhere {


  case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
    //  assert(!basedOn.isInstanceOf[ContextContentVariable])
    val label: VariableLabel = basedOn.label

    override val name = Name(basedOn.name.str + "☐" + index)
  }

}

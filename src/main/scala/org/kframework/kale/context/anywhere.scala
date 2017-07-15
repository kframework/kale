package org.kframework.kale.context

import org.kframework.kale._
import org.kframework.kale.context.anywhere.ContextContentVariable
import org.kframework.kale.context.pattern.{PatternContextApplication, PatternContextApplicationLabel}
import org.kframework.kale.standard.{HolesMixin, Name}
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunctions}
import org.kframework.kale.transformer.{Binary, Unary}
import org.kframework.kale.util.Named

trait ContextMixin extends Environment with standard.MatchingLogicMixin with HasMatcher {
  val Context = new Named("Context") with Label3 {
    override def apply(variable: Term, redex: Term, contextPredicate: Term = Or(And(anywhere, Variable.freshVariable()), Variable.freshVariable())): ContextApplication = variable match {
      case v: Variable => ContextApplication(v, redex, contextPredicate)
      case env.ForAll(v: Variable, _) => ContextApplication(v, redex, contextPredicate)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + variable)
    }

    val hole = Variable("CONTEXT_HOLE")

    val anywhere = (new Named("anywhere") with Label0 {
      override def apply(): Term = new FreeNode0(this) {
        override lazy val isPredicate = true
      }
    }) ()

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }

  val SolvingContext = new Named("SolvingContext") with Label1 {
    override def apply(_1: Term): Term = {
      assert(_1.label == Context)
      FreeNode1(this, _1)
    }
  }

  def ANYWHERE(t: Term) = Context(Variable.freshVariable(), t, And(Variable.freshVariable(), Context.anywhere))

  case class ContextApplication(contextVar: Variable, redex: Term, contextPredicate: Term) extends Node3 with Context {

    val label = Context

    val _1: Variable = contextVar
    val _2: Term = redex
    val _3: Term = contextPredicate
    val specificHole: ContextContentVariable = label.hole(contextVar)

    private val unfoldedContextPredicate = contextPredicate.mapBU({
      case Context.hole => SolvingContext(this);
      case o => o
    })

    val finalContextPredicate = contextPredicate.variables.filter(_ != Context.hole).foldLeft(unfoldedContextPredicate) {
      case (t, v) => Exists(v, t)
    }

    override lazy val isGround = false
  }

  def ContextMatcher(solver: Apply): (ContextApplication, Term) => Term = { (contextApp: ContextApplication, term: Term) =>
    solver(SolvingContext(contextApp), term).asOr map {
      case And.SPO(s, p@And.set(setOfp), Next(n)) =>
        val redex = setOfp.collect({
          case Exists(contextApp.specificHole, r) => r
        }).head
        And.SPO(
          And.substitution(s.asMap + (contextApp.contextVar -> n)),
          And(setOfp.filter({ case Exists(contextApp.specificHole, _) => false; case _ => true })),
          Next(Equality.binding(contextApp.specificHole, redex)(n)))
    }
  }

  def SolvingContextMatcher(solver: Apply): (Node1, Term) => Term = { (solvingContext: Node1, term: Term) =>
    val contextApp = solvingContext._1.asInstanceOf[ContextApplication]

    assert(contextApp.label == Context)
    val contextVar = contextApp.contextVar

    def solutionFor(subterms: Seq[Term], reconstruct: (Int, Term) => Term, avoidIndices: Set[Int] = Set()) = {
      Or((subterms.indices.toSet &~ avoidIndices) map { i: Int =>
        // calling f directly instead of solver because we know contextApp is hooked to the current f
        val solutionForSubtermI = solver(solvingContext, subterms(i))
        solutionForSubtermI.asOr map {
          // this rewires C -> HOLE into C -> foo(HOLE)
          case And.SPO(s, p, Next(next)) =>
            And.SPO(s, p, Next(reconstruct(i, next)))
        }
      })
    }

    term.label match {
      case Context =>
        val (rightContextVar, rightContextRedex, rightContextPredicate) = Context.unapply(term).get
        solutionFor(term.children.toSeq, (_: Int, tt: Term) => Context(rightContextVar, tt, rightContextPredicate), Set(0, 2))
      case `Or` => {
        term.asOr map (solver(contextApp, _))
      }
      case `And` => {
        ???
      }
      case other =>
        val matchPredicate = unify(contextApp.finalContextPredicate, term)

        val res = matchPredicate.asOr map {
          case And.SPO(_, p, Next(_)) if p.contains(Context.anywhere) =>
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
          case And.SPO(s, p, Next(n)) if p.findBU({ case Exists(contextApp.specificHole, _) => true; case _ => false }).isEmpty =>
            val redexSol = solver(contextApp.redex, n)
            redexSol.asOr map {
              case And.SPO(ss, pp, Next(redexTerm)) =>
                And.SPO(And.substitution(s.asMap ++ ss.asMap), And(p, pp, Exists(contextApp.specificHole, redexTerm)), Next(contextApp.specificHole))
            }
          case o => o
        }
        res
    }
  }
}

// TODO: un-bundle after we have decoupled the unary functions (substitution)
trait BundledContextMixin extends HolesMixin with ContextMixin with PatternContextMixin {

  object AnywhereContextProcessingFunction extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = ContextApplication

    override def f(solver: SubstitutionApply)(t: ContextApplication): Term = {
      val recursiveResult = Equality.binding(t.specificHole, solver(t.redex))
      And(solver.substitution, recursiveResult) match {
        case And.withNext(subs: Substitution, _) =>
          val innerSolver = new SubstitutionWithContext(subs)

          solver.substitution.get(t.contextVar) map innerSolver getOrElse Context(t.contextVar, solver(t.redex), solver(t.contextPredicate))
        case `Bottom` => Bottom
        case _ => t // TODO: risky case; look into this at some point
      }
    }
  }

  case class SubstitutionWithContext(override val substitution: Substitution) extends SubstitutionApply(substitution)(env) {
    override def processingFunctions: ProcessingFunctions = definePartialFunction[Term, this.type]({
      case Context => AnywhereContextProcessingFunction
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
    case (Context, _) => ContextMatcher
    case (SolvingContext, _) => SolvingContextMatcher
  }).orElse(super.makeMatcher)
}

object anywhere {


  case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
    //  assert(!basedOn.isInstanceOf[ContextContentVariable])
    val label: VariableLabel = basedOn.label

    override val name = Name(basedOn.name.str + "☐" + index)
  }

}

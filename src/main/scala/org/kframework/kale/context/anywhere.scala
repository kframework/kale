package org.kframework.kale.context

import org.kframework.kale.{Environment, HasMatcher, _}
import org.kframework.kale.context.anywhere.ContextContentVariable
import org.kframework.kale.standard.{HolesMixin, Name}
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunctions}
import org.kframework.kale.transformer.{Binary, Unary}
import org.kframework.kale.util.{LabelNamed, timer}
import org.roaringbitmap.RoaringBitmap

trait ContextMixin extends Mixin {
  _: Environment with standard.MatchingLogicMixin =>

  val Context = new LabelNamed("Context") with Label3 {
    override val isPredicate: Option[Boolean] = Some(false)

    override def requiredLabels(children: Iterable[Term]) = Roaring.requiredFor(children.tail)

    override def suppliedLabels(children: Iterable[Term]) = allLabelIds

    override def apply(variable: Term, redex: Term, contextPredicate: Term = Or(And(anywhere, Variable.freshVariable()), Variable.freshVariable())): ContextApplication = variable match {
      case v: Variable => ContextApplication(v, redex, contextPredicate)
      case env.ForAll(v: Variable, _) => ContextApplication(v, redex, contextPredicate)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + variable)
    }

    val hole = Variable("CONTEXT_HOLE")

    val anywhere = (new LabelNamed("anywhere") with Label0 with CluelessRoaring {
      override val isPredicate: Option[Boolean] = Some(false)

      override def apply(): Term = new FreeNode0(this) {
        override lazy val isPredicate = true
      }
    }) ()

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }

  val SolvingContext = new LabelNamed("SolvingContext") with Label1 with Projection1Roaring {
    override val isPredicate: Option[Boolean] = Some(false)

    override def apply(_1: Term): Term = {
      assert(_1.label == Context)
      FreeNode1(this, _1)
    }
  }

  def ANYWHERE(t: Term) = Context(Variable.freshVariable(), t)

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
      case And.SPN(s, p@And.set(setOfp), n) =>
        val redex = setOfp.collect({
          case Exists(contextApp.specificHole, r) => r
        }).head
        And.SPN(
          And.substitution(s.asMap + (contextApp.contextVar -> n)),
          And(setOfp.filter({ case Exists(contextApp.specificHole, _) => false; case _ => true })),
          Equality.binding(contextApp.specificHole, redex)(n))
    }
  }

  def indicesToAvoidTraversingForTerm(t: Term): Set[Int] = Set()

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
          case And.SPN(s, p, next) =>
            And.SPN(s, p, reconstruct(i, next))
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

        matchPredicate.asOr map {
          case And.SPN(s, p, n) if p.contains(Context.anywhere) =>
            val theAnywhereMatch = other match {
              //              case l: AssocLabel =>
              //                val subresults = l.asIterable(term).toList
              //                val recursive = solutionFor(subresults, (pos: Int, tt: Term) => l(subresults.updated(pos, tt)))
              //                recursive
              case l =>
                // C[bar(X)] := foo(bar(1))
                val subterms = term.children
                val recursive = solutionFor(subterms.toSeq, (pos: Int, tt: Term) => term.updateAt(pos)(tt), indicesToAvoidTraversingForTerm(term))
                And(s, recursive)
            }
            theAnywhereMatch
          case And.SPN(s, p, n) if p.findBU({ case Exists(contextApp.specificHole, _) => true; case _ => false }).isEmpty =>
            val redexSol = solver(contextApp.redex, n)
            redexSol.asOr map {
              case And.SPN(ss, pp, redexTerm) =>
                And.SPN(And.substitution(s.asMap ++ ss.asMap), And(p, pp, Exists(contextApp.specificHole, redexTerm)), contextApp.specificHole)
            }
          case o => o
        }
    }
  }
}

// TODO: un-bundle after we have decoupled the unary functions (substitution)
trait BundledContextMixin extends ContextMixin with PatternContextMixin {
  _: Environment with HolesMixin with standard.MatchingLogicMixin =>

  object AnywhereContextProcessingFunction extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = ContextApplication

    override def f(solver: SubstitutionApply)(t: ContextApplication): Term = {
      val recursiveResult = Equality.binding(t.specificHole, solver(t.redex))
      And(solver.substitution, recursiveResult) match {
        case And.SPN(subs, _, _) =>
          val innerSolver = new SubstitutionWithContext(subs)

          solver.substitution.get(t.contextVar) map innerSolver getOrElse Context(t.contextVar, solver(t.redex), solver(t.contextPredicate))
        case `Bottom` => Bottom
        case _ => t // TODO: risky case; look into this at some point
      }
    }
  }

  object RewriteProcessingFunction extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = Rewrite

    def f(solver: SubstitutionApply)(rw: Rewrite): Term = {
      val rhsSolver = substitutionMaker(And(solver.substitution.asMap.filter(_._2.isGround)))
      rw.copy(solver(rw._1), rhsSolver(rw._2))
    }
  }

  case class SubstitutionWithContext(override val substitution: Substitution) extends SubstitutionApply(substitution)(env) {
    override def processingFunctions: ProcessingFunctions = definePartialFunction[Term, this.type]({
      case Context => AnywhereContextProcessingFunction
      case `Rewrite` => RewriteProcessingFunction
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


  register(Binary.definePartialFunction({
    case (capp: PatternContextApplicationLabel, _) => PatternContextMatcher
    case (Context, _) => ContextMatcher
    case (SolvingContext, _) => SolvingContextMatcher
  }), Priority.high + 1)
}

object anywhere {


  case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
    //  assert(!basedOn.isInstanceOf[ContextContentVariable])
    val label: VariableLabel = basedOn.label

    override val name = Name(basedOn.name.str + "☐" + index)
  }

}

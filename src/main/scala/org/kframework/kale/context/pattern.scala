package org.kframework.kale.context

import org.kframework.kale._
import org.kframework.kale.context.anywhere.ContextContentVariable
import org.kframework.kale.standard.{CurrentEnvironment, SubstitutionWithContext}
import org.kframework.kale.transformer.Binary.TypedWith
import org.kframework.kale.transformer.{Binary, Unary}
import org.kframework.kale.util.Util

import scala.collection.{Map, Set}

object pattern {

  case class PatternContextApplicationLabel(name: String)(implicit val env: CurrentEnvironment) extends Context1ApplicationLabel {

    //  val C = env.Variable("GENERIC_CONTEXT_VAR")

    var patterns: Term = null

    def setPatterns(ps: Term) {
      this.patterns = ps

      assert(env.Or.asSet(patterns) map {
        case env.Equality(_, env.And.formulasAndNonFormula(_, Some(_))) => true;
        case _ => false
      } reduce (_ && _))
    }

    override def apply(_1: Term, _2: Term): PatternContextApplication
    = _1 match {
      case v: Variable => PatternContextApplication(this, v, _2)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + _1)
    }

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }

  case class PatternContextApplication(label: PatternContextApplicationLabel, contextVar: Variable, redex: Term) extends Node2 with Context {
    override def _1: Term = contextVar

    override def _2: Term = redex

    import label.env._

    private val sub = And.substitution(Map(Hole -> redex))

    def contextVariables(t: Term): Set[Variable] = t match {
      case c: Context => Set(c.contextVar)
      case Node(_, cs) => (cs flatMap contextVariables).toSet
      case _ => Set()
    }

    lazy val patternsWithRedexHolesAndTheirContextVariables: Set[(Term, Term, Set[Variable])] = Or.asSet(label.patterns) map {
      case Equality(left, right) =>
        (Equality(sub(left), sub(right)), right, Util.variables(right))
    }
  }

  class PatternContextMatcher(implicit env: CurrentEnvironment) extends transformer.Binary.ProcessingFunction[Binary.Apply] with TypedWith[PatternContextApplication, Term] {

    import env._
    import org.kframework.kale.util.StaticImplicits._

    override def f(solver: Binary.Apply)(contextApplication: PatternContextApplication, term: Term): Term = {
      val leftContextLabel = contextApplication.label
      val contextVar = contextApplication.contextVar
      val redex = contextApplication.redex


      Or(contextApplication.patternsWithRedexHolesAndTheirContextVariables map {
        case (Equality(And.formulasAndNonFormula(leftFormulas, Some(theContextDeclaration)), right), withHoles, contextVars) =>
          val contextMatch = solver(right, term)
          val contextMatchSolutions = Or.asSet(contextMatch)
          Or(contextMatchSolutions map {
            case And.substitutionAndTerms(sub@And.substitution(substitutionAsAMap), rhsLeftoverConstraints) =>
              val partiallySolvedLeftFormulas = sub(leftFormulas)
              val contextSub = Equality(contextVar, sub(withHoles))
              // TODO: filter out less
              And(partiallySolvedLeftFormulas, contextSub, And.substitution(substitutionAsAMap.filter({ case (k, _) => !contextVars.contains(k) })))
          })
      })

      // `buz(H)`[H] = buz(C[H])
      // `H`[H] = H
      // foo(C[bar(X)])
      // foo(buz(bar(1)))
      // C -> buz(H)
      // X -> 1

      // foo(...)
      // ... solving C[bar(X)]
      //     ... matching context C[H] buz(bar(X))
      //         ... regular match buz(...) upto matching context C[H] bar(1)
      //             ... H bar(1) ==> H -> bar(1)
      //             C -> H, H -> bar(1)
      //         C -> buz(H), H -> bar(1)
      //     ... matching bar(X) bar(1)
      //     C -> buz(H), H -> bar(1), X -> 1
    }
  }

  class PatternContextProcessingFunction(implicit env: CurrentEnvironment) extends Unary.ProcessingFunction[SubstitutionApply] {
    type Element = PatternContextApplication

    import env._

    override def f(solver: SubstitutionApply)(t: PatternContextApplication): Term = {
      val recursiveResult = Equality.binding(Hole, solver(t.redex))
      And.substitution(solver.substitution, recursiveResult) match {
        case subs: Substitution =>
          val innerSolver = new SubstitutionWithContext(subs)(env)

          solver.substitution.get(t.contextVar) map innerSolver getOrElse Bottom
        case `Bottom` => Bottom
      }
    }
  }

}
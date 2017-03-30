package org.kframework.kale

import collection._

object context {

  trait ContextLabel extends Label

  trait Context1ApplicationLabel extends Label2 with ContextLabel {
    def hole(x: Variable): ContextContentVariable

    override def unapply(t: Term): Option[(Variable, Term)] = t match {
      case n: Context1Application if n.label == this => Some(n._1, n._2)
      case _ => None
    }
  }

  case class AnywhereContextApplicationLabel(implicit val env: Environment) extends Named("AnywhereContext") with Context1ApplicationLabel {
    override def apply(_1: Term, _2: Term): Context1Application = _1 match {
      case v: Variable => Context1Application(this, v, _2)
      case _ => throw new AssertionError(id + " " + "First parameter needs to be a variable but was: " + _1)
    }

    def hole(x: Variable) = ContextContentVariable(x, 1)
  }

  case class PatternContextApplicationLabel(name: String)(implicit val env: CurrentEnvironment) extends Context1ApplicationLabel {

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

  trait Context extends Term {
    val contextVar: Variable
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

  case class Context1Application(label: Context1ApplicationLabel, contextVar: Variable, redex: Term) extends Node2 with Context {
    val _1: Variable = contextVar
    val _2: Term = redex
    val hole: ContextContentVariable = label.hole(contextVar)
    override lazy val isGround = false
  }

  case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
    //  assert(!basedOn.isInstanceOf[ContextContentVariable])
    val label: VariableLabel = basedOn.label

    override val name: String = basedOn.name + "_" + index
  }

  class PatternContextMatcher(implicit env: CurrentEnvironment) extends transformer.Binary.ProcessingFunction[PatternContextApplication, Term, Term] {

    import env._

    import StaticImplicits._

    override def f(solver: transformer.Binary.State)(contextApplication: PatternContextApplication, term: Term): Term = {
      val leftContextLabel = contextApplication.label.asInstanceOf[PatternContextApplicationLabel]
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

  class AnywhereContextMatcher(implicit env: CurrentEnvironment) extends transformer.Binary.ProcessingFunction[Context1Application, Term, Term] {

    import env._

    override def f(solver: transformer.Binary.State)(contextApplication: Context1Application, term: Term): Term = {
      assert(contextApplication.label == AnywhereContext)
      val contextVar = contextApplication.contextVar

      def solutionFor(subterms: List[Term], reconstruct: (Int, Term) => Term) = {
        Or(subterms.indices map { i =>
          // calling f directly instead of solver because we know contextApplication is hooked to the current f
          val solutionForSubtermI = f(solver)(contextApplication, subterms(i))
          val res = Or.asSet(solutionForSubtermI) map {
            // this rewires C -> HOLE into C -> foo(HOLE)
            case And.substitution(m) if m.contains(contextVar) => And.substitution(m.updated(contextVar, reconstruct(i, m(contextVar))))
          }
          Or(res)
        })
      }

      term.label match {
        case AnywhereContext =>
          val (rightContextVar, rightContextTerm) = AnywhereContext.unapply(term).get
          def findMatches(t: Term): Term = {
            Or(t match {
              case AnywhereContext(_, tt) => solver(contextApplication.redex, tt)
              case tt => Or(t.map(findMatches))
            }, solver(contextApplication.redex, t))
          }

          val recursive = findMatches(rightContextTerm)
          Or(Or.asSet(recursive) map {
            case And.substitution(m) => And.substitution(m.updated(contextVar, rightContextVar))
          })
        case l: AssocLabel =>
          val zeroLevel: Term = And(solver(contextApplication.redex, term), Equality(contextApplication.contextVar, contextApplication.hole))
          val subresults = l.asList(term).toList
          val recursive = solutionFor(subresults, (pos: Int, tt: Term) => l(subresults.updated(pos, tt)))
          Or(recursive, zeroLevel)
        case l =>
          // C[bar(X)] := foo(bar(1))

          val zeroLevel: Term = And(
            // zero level tries to match bar(X) with foo(bar(X))
            solver(contextApplication.redex, term),
            // C -> HOLE
            Equality(contextApplication.contextVar, contextApplication.hole))
          val subterms = term.toList
          val recursive = solutionFor(subterms, (pos: Int, tt: Term) => term.updateAt(pos)(tt))
          Or(recursive, zeroLevel)
      }
    }
  }

}

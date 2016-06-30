package org.kframework.kale

trait ContextLabel extends Label

trait Context1Label extends Label2 with UniqueId with ContextLabel {
  def hole(x: Variable): ContextContentVariable

  override def unapply(t: Term): Option[(Variable, Term)] = t match {
    case n: Context1 if n.label == this => Some(n._1, n._2)
    case _ => None
  }
}

object AnywhereContext extends Context1Label {
  val name = "AnywhereContext"

  override def apply(_1: Term, _2: Term): Context1 = _1 match {
    case v: Variable => Context1(this, v, _2)
    case _ => throw new AssertionError("First parameter needs to be a variable but was: " + _1)
  }

  def hole(x: Variable) = ContextContentVariable(x, 1)
}

trait Context extends Term

case class Context1(label: Context1Label, contextVar: Variable, term: Term) extends Node2 with Context {
  val _1 = contextVar
  val _2 = term
  val hole = label.hole(contextVar)
  override lazy val isGround = false
}

case class ContextContentVariable(basedOn: Variable, index: Int) extends Variable {
  //  assert(!basedOn.isInstanceOf[ContextContentVariable])

  override val name: String = basedOn.name + "_" + index
}

object AnywhereContextMatcher extends transformer.Binary.Function[Context1, Term, Term] {
  override def f(solver: transformer.Binary.State)(leftContext: Context1, t: Term): Term = {
    assert(leftContext.label == AnywhereContext)
    val v = leftContext.contextVar

    def solutionFor(subterms: List[Term], reconstruct: (Int, Term) => Term) = {
      Or(subterms.indices map { i =>
        val solutionForSubtermI = solver(leftContext, subterms(i))
        val res = Or.unwrap(solutionForSubtermI) map {
          case Substitution.map(m) if m.contains(v) => Substitution(m.updated(v, reconstruct(i, m(v))))
        }
        Or(res)
      })
    }

    t.label match {
      case AnywhereContext =>
        val (rightContextVar, rightContextTerm) = AnywhereContext.unapply(t).get
        def findMatches(t: Term): Term = {
          Or(t match {
            case AnywhereContext(_, tt) => solver(leftContext.term, tt)
            case tt => Or(t.map(findMatches))
          }, solver(leftContext.term, t))
        }

        val recursive = findMatches(rightContextTerm)
        Or(Or.unwrap(recursive) map {
          case Substitution.map(m) => Substitution(m.updated(v, rightContextVar))
        })
      case l: AssocLabel =>
        val zeroLevel: Term = And(solver(leftContext.term, t), Equality(leftContext.contextVar, leftContext.hole))
        val subresults = l.asList(t).toList
        val recursive = solutionFor(subresults, (pos: Int, tt: Term) => l(subresults.updated(pos, tt)))
        Or(recursive, zeroLevel)
      case l =>
        val zeroLevel: Term = And(solver(leftContext.term, t), Equality(leftContext.contextVar, leftContext.hole))
        val subterms = t.toList
        val recursive = solutionFor(subterms, (pos: Int, tt: Term) => t.updateAt(pos + 1)(tt))
        Or(recursive, zeroLevel)
    }
  }
}

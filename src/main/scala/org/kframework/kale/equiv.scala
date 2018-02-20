package org.kframework.kale

object equiv {

  import cats._
  import cats.implicits._

  implicit object EqMemo extends Monoid[EqMemo] {
    override def empty: EqMemo = new EqMemo(Set())

    override def combine(x: EqMemo, y: EqMemo): EqMemo = {
      new EqMemo(x.s ++ y.s)
    }

    def apply(x: Term, y: Term) = new EqMemo(Set((x, y)))
  }

  class EqMemo(protected val s: Set[(Term, Term)]) extends PartialEq[Term] {
    val m1 = s map (_._1)
    val m2 = s map (_._2)

    override def isDefinedAt(p: (Term, Term)): Boolean = m1.contains(p._1) || m2.contains(p._2)

    override def apply(p: (Term, Term)): Eq[Term] = (x: Term, y: Term) => s.contains(p)
  }

  trait PartialEq[T] extends PartialFunction[(T, T), Eq[T]]

  /*
    Should should the term be handled by the alpha-equivalence mechanism?
    If yes, it assumes the corresponding pattern is the only possible equivalent pattern.
    Useful, for now, only for marking variable renaming.
    E.g., for program variables (language-level, not K/meta-level), the trait should return true
    if the terms are both variables and have the same name.
   */
  trait CanBeEquivalent extends ((Term, Term) => Boolean)

  implicit def TermEq(implicit canBeEquiv: CanBeEquivalent) = new Eq[Term] {
    override def eqv(x: Term, y: Term): Boolean = innerEqv(EqMemo.empty)(x, y).isDefined

    /**
      * None represents not equivalent
      * Some(eq) means we still think we are equivalent and traversing further using eq
      */
    private def innerEqv(knownEq: EqMemo)(x: Term, y: Term): Option[EqMemo] = {
      if (canBeEquiv(x, y)) {
        // activate the equivalency mechanism
        if (knownEq.isDefinedAt((x, y))) {
          if (knownEq(x, y).eqv(x, y)) Some(knownEq) else None
        } else {
          Some(knownEq combine EqMemo(x, y))
        }
      } else {
        // equivalency mechanism not activated
        // check label equality, recurse on children if necessary
        if (x.label != y.label)
          None
        else {
          x.label match {
            case _: NodeLabel =>
              x.flattenedChildren.zip(y.flattenedChildren)
                .foldLeft(Some(knownEq): Option[EqMemo]) {
                  case (None, _) => None
                  case (Some(eqM), (a, b)) => innerEqv(eqM)(a, b)
                }
            case _: LeafLabel[_] =>
              if (x == y) Some(knownEq) else None
          }
        }
      }
    }
  }


  implicit def alphaEquivalenceBasedOrder(implicit canBeEquivalent: CanBeEquivalent) = new Order[Term] {
    override def compare(x: Term, y: Term): Int = {
      if (implicitly[Eq[Term]].eqv(x, y)) {
        0
      } else {
        val labelEq = x.label.name compareTo y.label.name
        if (labelEq == 0) {
          x.label match {
            case _: NodeLabel => x.flattenedChildren.zip(y.flattenedChildren).collectFirst({
              case (a, b) if compare(a, b) != 0 => compare(a, b)
            }).get
            case l: LeafLabel[_] =>
              x.toString compareTo y.toString
          }
        } else {
          labelEq
        }
      }
    }
  }
}


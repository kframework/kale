package org.kframework.kale

trait Context1Label extends Label2 with UniqueId

object AnywhereContext extends Context1Label {
  val name = "ANYWHERE"

  override def apply(_1: Term, _2: Term): Term = _1 match {
    case v: Variable => Context1(this, v, _2)
    case _ => throw new AssertionError("First parameter needs to be a variable")
  }
}

case class Context1(label: Context1Label, contextVar: Variable, term: Term) extends Node2 {
  val _1 = contextVar
  val _2 = term
}

object AnywhereContextMatcher extends UnifierFunction[Context1, Term, Term] {
  override def f(solver: DispatchState)(c: Context1, t: Term): Term = {
    assert(c.label == AnywhereContext)

    val zeroLevel = solver(c.term, t)

    Equality(c.contextVar, zeroLevel)
  }
}

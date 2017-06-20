package org.kframework.kale.strategy

import org.kframework.kale.builtin.Module
import org.kframework.kale.pretty.PrettyWrapperHolder
import org.kframework.kale.standard.{SingleSortedMatcher, StandardEnvironment}
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.Named
import org.kframework.kale.{Environment, FreeNode1, FreeNode2, FunctionLabel1, Label, Label1, Label2, Node1, Node2, Term, Variable, standard}

case class STRATEGY1()(implicit penv: StandardEnvironment) extends Module("STRATEGY") {

  import penv._

  val orElse = new Named("orElse") with Label2 {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  val nextIsNow = standard.lift("nextIsNow", penv.And.nextIsNow _)

  val onlyNext = standard.lift("onlyNext", penv.And.onlyNext _)

  val functionReference = new Named("functionReference") with Label1 {
    override def apply(_1: Term): Term = {
      val freshVar = Variable.freshVariable()
      unappliedFunctionReference(freshVar, Equality.binding(Hole, freshVar)(_1))
    }
  }

  val unappliedFunctionReference = new Named("unappliedFunctionReference") with Label2 {
    override def apply(_1: Term, _2: Term): Term = {
      FunctionReference(_1.asInstanceOf[Variable], _2)
    }
  }

  private case class FunctionReference(_1: Variable, _2: Term) extends Node2 {
    override val label = unappliedFunctionReference
    override val isPredicate: Boolean = false
  }

  val compose = new Named("compose") with Label2 {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  val star = new Named("star") with FunctionLabel1 {
    override def f(_1: Term): Option[Term] = _1 match {
      case Hole => None
      case _ => Some(starImpl(_1))
    }
  }

  def starImpl(t: Term): Term = orElse(compose(functionReference(star(Hole)), t), Variable.freshVariable())

  val repeat = new Named("repeat") with Label1 {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  val fixpoint = new Named("fixpoint") with Label1 {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  override val all: Set[Label] = Set()
}

trait importSTRATEGY {
  protected val env: StandardEnvironment


  val STRATEGY = org.kframework.kale.strategy.STRATEGY1()(env)
}

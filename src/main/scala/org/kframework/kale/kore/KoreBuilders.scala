package org.kframework.kale.kore

import org.kframework.kale._
import org.kframework.minikore.interfaces.build.Builders
import org.kframework.minikore.interfaces.pattern

import scala.collection.Seq

class KoreBuilders(implicit val env: Environment) extends Builders {

  def Symbol(str: String): pattern.Symbol = env.label(str)

  def Variable(_1: pattern.Name, _2: pattern.Sort): pattern.Variable = free.SimpleVariable(_1)

  def DomainValue(_1: pattern.Symbol, _2: pattern.Value): pattern.DomainValue = _1.asInstanceOf[ConstantLabel[_]].interpret(_2)

  def Top(): pattern.Top = env.Top

  def Bottom(): pattern.Bottom = env.Bottom

  def Not(_1: pattern.Pattern): pattern.Pattern = ???

  def Next(_1: pattern.Pattern): pattern.Pattern = ???

  def And(_1: pattern.Pattern, _2: pattern.Pattern): pattern.Pattern = env.And(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  def Or(_1: pattern.Pattern, _2: pattern.Pattern): pattern.Pattern = env.Or(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  def Implies(_1: pattern.Pattern, _2: pattern.Pattern): pattern.Pattern = ???

  def Equals(_1: pattern.Pattern, _2: pattern.Pattern): pattern.Pattern = env.Equality(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  def Exists(_1: pattern.Variable, _2: pattern.Pattern): pattern.Pattern = ???

  def ForAll(_1: pattern.Variable, _2: pattern.Pattern): pattern.Pattern = ???

  def Rewrite(_1: pattern.Pattern, _2: pattern.Pattern): pattern.Pattern = env.Rewrite(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  def Application(_1: pattern.Symbol, args: Seq[pattern.Pattern]): pattern.Pattern = _1.asInstanceOf[NodeLabel](args.asInstanceOf[Seq[Term]])
}

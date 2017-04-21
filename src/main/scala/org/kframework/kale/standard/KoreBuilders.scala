package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kore

import scala.collection.Seq

class KoreBuilders(implicit val env: Environment) extends kore.Builders {

  override def Symbol(str: String): kore.Symbol = env.label(str)

  override def SortedVariable(_1: kore.Name, _2: kore.Sort): kore.Variable = StandardVariable(_1.asInstanceOf[Name], _2.asInstanceOf[Sort])

  override def DomainValue(_1: kore.Symbol, _2: kore.Value): kore.DomainValue = {
    def instantiate[T]() = _1.asInstanceOf[DomainValueLabel[T]].interpret(_2.asInstanceOf[Value[T]])

    instantiate()
  }

  override def Top(): kore.Top = env.Top

  override def Bottom(): kore.Bottom = env.Bottom

  override def Not(_1: kore.Pattern): kore.Pattern = ???

  override def Next(_1: kore.Pattern): kore.Pattern = ???

  override def And(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.And(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Or(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.Or(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Implies(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = ???

  override def Equals(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.Equality(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Exists(_1: kore.Variable, _2: kore.Pattern): kore.Pattern = ???

  override def ForAll(_1: kore.Variable, _2: kore.Pattern): kore.Pattern = ???

  override def Rewrite(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.Rewrite(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Application(_1: kore.Symbol, args: Seq[kore.Pattern]): kore.Pattern = _1.asInstanceOf[NodeLabel](args.asInstanceOf[Seq[Term]])

  override def Definition(modules: Seq[kore.Module], att: kore.Attributes): kore.Definition = ???

  override def Module(name: kore.ModuleName, sentences: Seq[kore.Sentence], att: kore.Attributes): kore.Module = ???

  override def Import(name: kore.ModuleName, att: kore.Attributes): kore.Import = ???

  override def SortDeclaration(sort: kore.Sort, att: kore.Attributes): kore.SortDeclaration = ???

  override def SymbolDeclaration(sort: kore.Sort, symbol: kore.Symbol, args: Seq[kore.Sort], att: kore.Attributes): kore.SymbolDeclaration = ???

  override def Rule(p: kore.Pattern, att: kore.Attributes): kore.Rule = ???

  override def Axiom(p: kore.Pattern, att: kore.Attributes): kore.Axiom = ???

  override def Attributes(att: Seq[kore.Pattern]): kore.Attributes = ???

  override def ModuleName(str: String): kore.ModuleName = ???

  override def Sort(str: String): kore.Sort = ???

  override def Value(str: String): kore.Value = ???

  override def Name(str: String): Name = ???
}

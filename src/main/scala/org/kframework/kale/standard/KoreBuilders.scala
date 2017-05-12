package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kore
import org.kframework.kore.implementation.DefaultBuilders

import scala.collection.Seq

class KoreBackend(d: kore.Definition, mainModule: kore.ModuleName) {
  val env = StandardEnvironment
}

trait KoreBuilders extends kore.Builders with DefaultOuterBuilders {

  implicit val env: Environment

  override def Symbol(str: String): kore.Symbol = env.label(str)

  override def SortedVariable(_1: kore.Name, _2: kore.Sort): kore.Variable = StandardVariable(_1.asInstanceOf[Name], _2.asInstanceOf[Sort])

  override def DomainValue(_1: kore.Symbol, _2: kore.Value): kore.Pattern = {
    def instantiate[T]() = _1.asInstanceOf[DomainValueLabel[T]].interpret(_2.str)

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

  def Sort(str: String): kore.Sort = standard.Sort(str)

  def Value(str: String): kore.Value = DefaultBuilders.Value(str)

  def Name(str: String): Name = standard.Name(str)
}

trait DefaultOuterBuilders {
  def Definition(att: kore.Attributes, modules: Seq[kore.Module]): kore.Definition = {
    DefaultBuilders.Definition(att, modules)
  }

  def Module(name: kore.ModuleName, sentences: Seq[kore.Sentence], att: kore.Attributes): kore.Module =
    DefaultBuilders.Module(name, sentences, att)

  def Import(name: kore.ModuleName, att: kore.Attributes): kore.Sentence =
    DefaultBuilders.Import(name, att)

  def SortDeclaration(sort: kore.Sort, att: kore.Attributes): kore.Sentence =
    DefaultBuilders.SortDeclaration(sort, att)

  def SymbolDeclaration(sort: kore.Sort, symbol: kore.Symbol, args: Seq[kore.Sort], att: kore.Attributes): kore.Sentence =
    DefaultBuilders.SymbolDeclaration(sort, symbol, args, att)

  def Rule(p: kore.Pattern, att: kore.Attributes): kore.Sentence = DefaultBuilders.Rule(p, att)

  def Axiom(p: kore.Pattern, att: kore.Attributes): kore.Sentence = DefaultBuilders.Axiom(p, att)

  def Attributes(att: Seq[kore.Pattern]): kore.Attributes = DefaultBuilders.Attributes(att)

  def ModuleName(str: String): kore.ModuleName = DefaultBuilders.ModuleName(str)
}

case class ConversionException(m: String) extends RuntimeException {
  override def getMessage: String = m
}


object StandardConverter {

  def apply(p: kore.Pattern)(implicit env: Environment): Term = p match {
    case kore.Application(kore.Symbol(s), args) => args match {
      case Seq() => SimpleFreeLabel0(s).apply()
      case Seq(p1) => SimpleFreeLabel1(s).apply(StandardConverter(p1))
      case Seq(p1, p2) => SimpleFreeLabel2(s).apply(StandardConverter(p1), StandardConverter(p2))
      case Seq(p1, p2, p3) => SimpleFreeLabel3(s).apply(StandardConverter(p1), StandardConverter(p2), StandardConverter(p3))
      case Seq(p1, p2, p3, p4) => SimpleFreeLabel4(s).apply(StandardConverter(p1), StandardConverter(p2), StandardConverter(p3), StandardConverter(p4))
    }
    case kore.SortedVariable(kore.Name(n), kore.Sort(s)) => StandardVariable(Name(n), Sort(s))
    case kore.Top() => standard.StandardTruthLabel().apply(true)
    case kore.Bottom() => standard.StandardTruthLabel().apply(false)
    case kore.Equals(p1, p2) => standard.StandardEqualityLabel().apply(StandardConverter(p1), StandardConverter(p2))
    case kore.And(p1, p2) => standard.DNFAndLabel().apply(StandardConverter(p1), StandardConverter(p2))
    case kore.Or(p1, p2) => standard.DNFOrLabel().apply(StandardConverter(p1), StandardConverter(p2))
    case kore.Not(p1) => standard.NotLabel().apply(StandardConverter(p1))
    case kore.Rewrite(p1, p2) => standard.Rewrite.apply(StandardConverter(p1), StandardConverter(p2))
    case kore.Implies(p1, p2) => DNFOrLabel().apply(NotLabel().apply(StandardConverter(p1)), StandardConverter(p2))
    case p@_ => throw ConversionException(p.toString + "Cannot Convert To Kale")
  }


}
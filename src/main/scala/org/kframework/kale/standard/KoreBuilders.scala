package org.kframework.kale.standard

import org.kframework.backend.skala.Encodings
import org.kframework.kale._
import org.kframework.kale.builtin._
import org.kframework.kore
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.DefaultBuilders

import scala.collection.Seq

class KoreBackend(d: kore.Definition, mainModule: kore.ModuleName) {
  val env = StandardEnvironment
}

trait KoreBuilders extends kore.Builders with DefaultOuterBuilders {

  implicit protected val env: Environment

  override def Symbol(str: String): kore.Symbol = env.label(str)

  override def SortedVariable(_1: kore.Name, _2: kore.Sort): kore.Variable = StandardVariable(_1.asInstanceOf[Name], _2.asInstanceOf[Sort])

  override def DomainValue(_1: kore.Symbol, _2: kore.Value): kore.Pattern = {
    def instantiate[T]() = _1.asInstanceOf[DomainValueLabel[T]].interpret(_2.str)

    instantiate()

  }

  override def Not(_1: kore.Pattern): kore.Pattern = ???

  override def Next(_1: kore.Pattern): kore.Pattern = ???

  override def And(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.And(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Or(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.Or(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Implies(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = ???

  override def Equals(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.Equality(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Exists(_1: kore.Variable, _2: kore.Pattern): kore.Pattern = ???

  override def ForAll(_1: kore.Variable, _2: kore.Pattern): kore.Pattern = ???

  override def Rewrite(_1: kore.Pattern, _2: kore.Pattern): kore.Pattern = env.Rewrite(_1.asInstanceOf[Term], _2.asInstanceOf[Term])

  override def Application(_1: kore.Symbol, args: Seq[kore.Pattern]): kore.Pattern = {
    env.label(_1.str) match {
      case l: NodeLabel => l(args.asInstanceOf[Seq[Term]])
      case _ => ???
    }
  }

  def Sort(str: String) = standard.Sort(str)

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


object EnvironmentImplicit {
  implicit def envToStdEnv(env: Environment): StandardEnvironment = env.asInstanceOf[StandardEnvironment]
}

object StandardConverter {


  //Todo: Special Cases Handle Generically
  val specialSymbolsSet: Set[String] = Set("#", "Map:lookup", "Set:in", "keys", "lookup")

  def apply(p: kore.Pattern)(implicit env: StandardEnvironment): Term = p match {
    case p@kore.Application(kore.Symbol(str), args) if specialSymbolsSet.contains(str) => specialPatternHandler(p)
    case kore.Application(kore.Symbol(s), args) =>
      val l = env.uniqueLabels(s).asInstanceOf[NodeLabel]
      val cargs = args.map(StandardConverter.apply)
      l(cargs)

    case kore.And(p1, p2) => env.And(StandardConverter(p1), StandardConverter(p2))
    case kore.Or(p1, p2) => env.Or(StandardConverter(p1), StandardConverter(p2))
    case kore.Top() => env.Top
    case kore.Bottom() => env.Bottom
    case kore.Equals(p1, p2) => env.Equality(StandardConverter(p1), StandardConverter(p2))
    case kore.SortedVariable(kore.Name(n), kore.Sort(s)) => n match {
      case "$PGM" => env.Variable(n, Sort("KConfigVar@BASIC-K"))
      case _ => env.Variable(n, Sort(s))
    }
    case kore.Not(p) => env.Not(StandardConverter(p))
    case kore.Rewrite(p1, p2) => env.Rewrite(StandardConverter(p1), StandardConverter(p2))
    case kore.DomainValue(symbol@kore.Symbol(s), value@kore.Value(v)) => env.label(s).asInstanceOf[DomainValueLabel[_]].interpret(v)
    case p@_ => throw ConversionException(p.toString + "Cannot Convert To Kale")
  }


  def apply(r: kore.Rule)(implicit env: StandardEnvironment): Term = r match {
    case kore.Rule(kore.Implies(requires, kore.And(kore.Rewrite(left, right), kore.Next(ensures))), att)
      if att.findSymbol(Encodings.macroEnc).isEmpty => {
      val convertedLeft = apply(left)
      val convertedRight = apply(right)
      val convetedRequires = apply(requires)
      val convertedEnsures = apply(ensures)
      env.Rewrite(env.And(convertedLeft, env.Equality(convetedRequires, env.toBoolean(true))), convertedRight)
    }
    case kore.Rule(kore.Implies(requires, kore.And(body, kore.Next(ensures))), att)
      if att.findSymbol(Encodings.macroEnc).isEmpty => {
      val convertedLeft = apply(body)
      val convetedRequires = apply(requires)
      val convertedEnsures = apply(ensures)
      import env._
      And(convertedLeft, env.Equality(convetedRequires, env.toBoolean(true)))
    }
    case _ => throw ConversionException("Encountered Non Uniform Rule")
  }

  //Todo: Better Mechanism To Handle These Cases
  private def specialPatternHandler(p: kore.Pattern)(implicit env: StandardEnvironment): Term = p match {
    case p@kore.Application(kore.Symbol(s), args) => s match {
      case "#" => apply(decodePatternAttribute(p)._1)
      case "Map:lookup" => env.label("_Map_").asInstanceOf[MapLabel].lookup(args.map(StandardConverter.apply))
      case "keys" => new KeysFunction(env.label("_Map_").asInstanceOf[MapLabel], env.label("_Set_").asInstanceOf[SetLabel]).apply(args.map(StandardConverter.apply))
    }
  }

  private def decodePatternAttribute(p: kore.Pattern): (kore.Pattern, Seq[kore.Pattern]) = {
    p match {
      case kore.Application(kore.Symbol("#"), Seq(p, p2)) => decodePatternAttribute(p) match {
        case (p1, a1) => (p1, p2 +: a1)
      }
      case p@_ => (p, Seq())
    }
  }
}
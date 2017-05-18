package org.kframework.backend.skala

import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.standard._
import org.kframework.kale.util.{Named, Util, fixpoint}
import org.kframework.kale.{Rewrite => _, _}
import org.kframework.kore
import org.kframework.kore.{Pattern, extended}
import org.kframework.kore.extended.{Backend, Rewriter}
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.DefaultBuilders

import scala.collection.Seq
import org.kframework.backend.skala.backendImplicits._


class SkalaBackend(implicit val env: StandardEnvironment, val originalDefintion: kore.Definition) extends KoreBuilders with extended.Backend {

  override def att: kore.Attributes = originalDefintion.att

  override def modules: Seq[kore.Module] = originalDefintion.modules

  lazy val rules: Set[Rewrite] = modules.flatMap(_.rules).filter(_.att.findSymbol(Encodings.function).isEmpty)
    .map(StandardConverter.apply).toSet

  lazy val substitutionApplier = SubstitutionWithContext(_)

  lazy val unifier: MatcherOrUnifier = SingleSortedMatcher()

  lazy val rewriterGenerator = Rewriter(substitutionApplier, unifier)

  lazy val rewriter = rewriterGenerator(rules)

  override def step(p: Pattern, steps: Int): Pattern = rewriter(p.asInstanceOf[Term]).toList.head
}

//Todo: Move somewhere else
object Encodings {
  val iMainModule = DefaultBuilders.Symbol("#MainModule")
  val iNone = DefaultBuilders.Symbol("#None")
  val assoc = DefaultBuilders.Symbol("assoc")
  val bag = DefaultBuilders.Symbol("bag")
  val relativeHook = DefaultBuilders.Symbol("relativeHook")
  val hook = DefaultBuilders.Symbol("hook")
  val function = DefaultBuilders.Symbol("function")
  val unit = DefaultBuilders.Symbol("unit")
  val index = DefaultBuilders.Symbol("index")
  val comm = DefaultBuilders.Symbol("comm")
  val macroEnc = DefaultBuilders.Symbol("macro")
  val rewrite = DefaultBuilders.Symbol("#KRewrite")
  val attributeValue = DefaultBuilders.Symbol("AttributeValue")
  val att = DefaultBuilders.Symbol("#")
}

object Hook {
  def apply(s: kore.SymbolDeclaration)(implicit env: StandardEnvironment): Option[Label] = {
    s.att.getSymbolValue(Encodings.hook) match {
      case Some(v) => v.str match {
        case "INT.Int" => Some(env.INT)
        case "INT.add" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.INT, (x: Int, y: Int) => x + y))
        case "INT.sub" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.INT, (x: Int, y: Int) => x - y))
        case "INT.ediv" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.INT, (x: Int, y: Int) => x / y))
        case "INT.tmod" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.INT, (x: Int, y: Int) => x % y))
        case "INT.abs" => Some(PrimitiveFunction1(s.symbol.str, env.INT, env.INT, (x: Int) => math.abs(x)))
        case "INT.shr" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.INT, (x: Int, y: Int) => x >> y))
        case "INT.not" => Some(PrimitiveFunction1(s.symbol.str, env.INT, env.INT, (x: Int) => ~x))
        case "INT.xor" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.INT, (x: Int, y: Int) => x ^ y))
        case "INT.ne" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.BOOLEAN, (x: Int, y: Int) => x != y))
        case "INT.gt" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.BOOLEAN, (x: Int, y: Int) => x > y))
        case "INT.ge" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.BOOLEAN, (x: Int, y: Int) => x >= y))
        case "INT.lt" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.BOOLEAN, (x: Int, y: Int) => x < y))
        case "INT.le" => Some(PrimitiveFunction2(s.symbol.str, env.INT, env.BOOLEAN, (x: Int, y: Int) => x <= y))
        case "BOOL.or" => Some(PrimitiveFunction2(s.symbol.str, env.BOOLEAN, env.BOOLEAN, (x: Boolean, y: Boolean) => x || y))
        case "BOOL.and" => Some(PrimitiveFunction2(s.symbol.str, env.BOOLEAN, env.BOOLEAN, (x: Boolean, y: Boolean) => x && y))
        case "BOOL.xor" => Some(PrimitiveFunction2(s.symbol.str, env.BOOLEAN, env.BOOLEAN, (x: Boolean, y: Boolean) => x ^ y))
        case "BOOL.ne" => Some(PrimitiveFunction2(s.symbol.str, env.BOOLEAN, env.BOOLEAN, (x: Boolean, y: Boolean) => x != y))
        case "BOOL.not" => Some(PrimitiveFunction1(s.symbol.str, env.BOOLEAN, (x: Boolean) => !x))
        case "BOOL.eq" => Some(PrimitiveFunction2(s.symbol.str, env.BOOLEAN, env.BOOLEAN, (x: Boolean, y: Boolean) => x == y))
        case "BOOL.implies" => Some(PrimitiveFunction2(s.symbol.str, env.BOOLEAN, env.BOOLEAN, (x: Boolean, y: Boolean) => !x || y))
        //Todo: How to handle these?
        case "BOOL.orElse" => Some(SimpleFreeLabel2(s.symbol.str))
        case "BOOL.andThen" => Some(SimpleFreeLabel2(s.symbol.str))
        case "KString" => Some(SimpleFreeLabel1(s.symbol.str))
        case "#KRewrite" => Some(FunctionDefinedByRewritingLabel2(s.symbol.str))
        case _ => None
      }
      case None => None
    }
  }
}


object DefinitionToStandardEnvironment extends (kore.Definition => StandardEnvironment) {

  import Encodings._

  import org.kframework.kore.implementation.{DefaultBuilders => db}

  def apply(d: kore.Definition): StandardEnvironment = {
    val mainModuleName: kore.ModuleName = {
      d.att.findSymbol(iMainModule) match {
        case Some(kore.Application(_, Seq(kore.DomainValue(kore.Symbol("S"), kore.Value(name)))))
        => DefaultBuilders.ModuleName(name)
        case None => ??? // throw exception
      }
    }

    val mainModule: kore.Module = d.modulesMap(mainModuleName)
    apply(d, mainModule)
  }

  private def isAssoc(s: kore.SymbolDeclaration): Boolean = {
    s.att.findSymbol(Encodings.assoc) match {
      case Some(_) => true
      case None => s.att.findSymbol(Encodings.bag) match {
        case Some(_) => true
        case None => false
      }
    }
  }


  def apply(d: kore.Definition, m: kore.Module): StandardEnvironment = {

    implicit val iDef = d

    case class IsSort(s: kore.Sort)(implicit env: Environment) extends Named(s.toString) with FunctionLabel1 {
      override def f(_1: Term): Option[Term] = {
        if (!_1.isGround)
          None
        else {
          val ss = m.sortsFor(db.Symbol(_1.label.name))
          ss.map(x => m.subsorts.<=(x, s)).filter(_)
          if (ss.nonEmpty) {
            Some(env.Truth(true))
          }
          None
        }
      }
    }



    val uniqueSymbolDecs: Seq[kore.SymbolDeclaration] = m.allSentences.collect({
      case sd@kore.SymbolDeclaration(_, s, _, _) if s != iNone => sd
    }).groupBy(_.symbol).mapValues(_.head).values.toSeq


    val sortDeclarations: Seq[kore.SortDeclaration] = m.sentences.collect({
      case s@kore.SortDeclaration(_, _) => s
    })


    val assocSymbols: Seq[kore.SymbolDeclaration] = uniqueSymbolDecs.filter(isAssoc)

    val nonAssocSymbols: Seq[kore.SymbolDeclaration] = uniqueSymbolDecs.diff(assocSymbols)

    implicit val env = StandardEnvironment()


    def declareSortPredicate(x: kore.SymbolDeclaration): Option[Label] = {
      Some(IsSort(db.Sort(x.symbol.str)).asInstanceOf[Label])
    }


    def declareNonHookedSymbol(x: kore.SymbolDeclaration): Option[Label] = {
      if (env.uniqueLabels.contains(x.symbol.str)) {
        None
      }
      else {
        x.att.findSymbol(Encodings.function) match {
          case Some(_) => {
            if (x.symbol.str.startsWith("is")) {
              declareSortPredicate(x)
            }

            //Functional Symbol Declaration
            x.args match {
              case Seq() => Some(FunctionDefinedByRewritingLabel0(x.symbol.str)(env))
              case Seq(_) => Some(FunctionDefinedByRewritingLabel1(x.symbol.str)(env))
              case Seq(_, _) => Some(FunctionDefinedByRewritingLabel2(x.symbol.str)(env))
              case Seq(_, _, _) => Some(FunctionDefinedByRewritingLabel3(x.symbol.str)(env))
              case Seq(_, _, _, _) => Some(FunctionDefinedByRewritingLabel4(x.symbol.str)(env))
            }
          }
          //
          case None => {
            // Non Functional Symbol Declaration
            x.args match {
              case Seq() => Some(SimpleFreeLabel0(x.symbol.str))
              case Seq(_) => Some(SimpleFreeLabel1(x.symbol.str))
              case Seq(_, _) => Some(SimpleFreeLabel2(x.symbol.str))
              case Seq(_, _, _) => Some(SimpleFreeLabel3(x.symbol.str))
              case Seq(_, _, _, _) => Some(SimpleFreeLabel4(x.symbol.str))
            }
          }
        }
      }
    }


    val hookedLabels: Set[Label] = nonAssocSymbols.flatMap(Hook(_)).toSet

    val unhookedLabels: Set[Label] = nonAssocSymbols.flatMap(declareNonHookedSymbol).toSet

    val nonAssocLabels = hookedLabels ++ unhookedLabels

    def getLabelForAtt(att: String): Label = {
      val label = nonAssocLabels.filter(p => p.name == att)
      assert(label.size == 1)
      label.head
    }


    // Initialize Assoc Labels.
    val assocLabels: Set[Label] = assocSymbols.flatMap(x => {
      val unitLabel: Option[Pattern] = x.att.findSymbol(Encodings.unit)
      val unitLabelValue: Option[String] = decodeAttributePattern(unitLabel, Encodings.unit.str)

      unitLabel match {
        case Some(_) => {
          env.uniqueLabels.get(x.symbol.str) match {
            case a@Some(_) => a
            case None => {
              val index: Option[Pattern] = x.att.findSymbol(Encodings.index)
              if (index.isDefined && x.att.findSymbol(Encodings.comm).isDefined) {
                // Both Commutative and Assoc with Index
                val indexStr: String = decodeAttributePattern(index, Encodings.index.str).get

                def indexFunction(t: Term): Term = t.children.toList(indexStr.toInt)

                // Create the AC Label with Identity Term
                Some(MapLabel(x.symbol.str, indexFunction, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
              } else {
                // Create the AssocLabel with Identity Term
                Some(new AssocWithIdListLabel(x.symbol.str, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
              }
            }
          }
        }
        case None => None
      }
    }).toSet



    // Function Rules

    val functionRulesAsLeftRight: Set[(Label, Rewrite)] = m.rules.collect({
      case r@kore.Rule(kore.Implies(_, kore.And(kore.Rewrite(kore.Application(kore.Symbol(label), _), _), _)), att) if att.findSymbol(Encodings.function).isDefined =>
        (env.label(label), StandardConverter(r))
    }).toSet


    val functionRules: Map[Label, Set[Rewrite]] = functionRulesAsLeftRight groupBy (_._1) map { case (k, set) => (k, set.map(_._2)) }

    val functionRulesWithRenamedVariables: Map[Label, Set[Rewrite]] = functionRules map { case (k, v) => (k, v map env.renameVariables) }


    env.seal()

//
//    lazy val substitutionApplier = SubstitutionWithContext(_)
//
//    lazy val unifier: MatcherOrUnifier = SingleSortedMatcher()
//
//    val rewriterGenerator = (s: Set[Rewrite]) => Rewriter(substitutionApplier, unifier)(s)
//
//    def setFunctionRules(functionRules: Map[Label, Set[Rewrite]]) {
//      env.labels.collect({
//        // TODO: Add an warning for when a function is not defined by either a hook or rules
//        case l: FunctionDefinedByRewriting => l.setRules(functionRules.getOrElse(l, Set()))(rewriterGenerator)
//      })
//    }
//
//    setFunctionRules(functionRulesWithRenamedVariables)
//
//    def reconstruct(inhibitForLabel: Label)(t: Term): Term = t match {
//      case Node(label, children) if label != inhibitForLabel => label(children map reconstruct(inhibitForLabel))
//      case t => t
//    }
//
//    def resolveFunctionRHS(functionRules: Map[Label, Set[Rewrite]]): Map[Label, Set[Rewrite]] = {
//      functionRules map { case (label, rewrites) => (label, rewrites map (rw => reconstruct(label)(rw).asInstanceOf[Rewrite])) }
//    }
//
//    val finalFunctionRules = fixpoint(resolveFunctionRHS)(functionRules)
//    setFunctionRules(finalFunctionRules)

    env
  }

  private def decodeAttributePattern(p: Option[Pattern], symbol: String): Option[String] = p match {
    case Some(kore.Application(kore.Symbol(`symbol`), Seq(kore.DomainValue(Encodings.`attributeValue`, kore.Value(v))))) => Some(v)
    case _ => None
  }

  private def decodePatternAttribute(p: Pattern): (Pattern, Seq[Pattern]) = {
    p match {
      case kore.Application(Encodings.`att`, Seq(p, p2)) => decodePatternAttribute(p) match {
        case (p1, a1) => (p1, p2 +: a1)
      }
      case p@_ => (p, Seq())
    }
  }

}

object SkalaBackend extends extended.BackendCreator {
  override def apply(d: kore.Definition): Backend = new SkalaBackend()(DefinitionToStandardEnvironment(d), d)

  // Todo: Use for Development, Replace with apply above
  def apply(d: kore.Definition, m: kore.Module): Backend = new SkalaBackend()(DefinitionToStandardEnvironment(d, m), d)
}


//class ScalaConverters(m: kore.Module)(implicit env: Environment) {
//  //Some Converters need to be ported here
//
//
//

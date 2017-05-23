package org.kframework.backend.skala

import org.kframework.backend.skala.backendImplicits._
import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.standard._
import org.kframework.kale.util.{Named, fixpoint}
import org.kframework.kale.{Rewrite => _, _}
import org.kframework.kore
import org.kframework.kore.extended.Backend
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Pattern, Rule, extended}

import scala.collection.mutable


//import scala.collection.{Seq, mutable}

class SkalaBackend(implicit val env: StandardEnvironment, implicit val originalDefintion: kore.Definition, val module: kore.Module) extends KoreBuilders with extended.Backend {

  override def att: kore.Attributes = originalDefintion.att

  override def modules: Seq[kore.Module] = module +: originalDefintion.modulesMap.get(module.name).get.imports

  val functionLabels: mutable.Map[String, Label] = env.uniqueLabels.filter(_._2.isInstanceOf[FunctionDefinedByRewriting])

  val functionalLabelRulesMap: Map[Label, Set[Rule]] = modules.flatMap(_.rules).collect({
    case r@kore.Rule(kore.Implies(_, kore.And(kore.Rewrite(kore.Application(kore.Symbol(label), _), _), _)), att) if functionLabels.contains(label) => (env.label(label), r)
  }).groupBy(_._1).mapValues(_.map(_._2).toSet)

  /**
    * At this point, all symbols (including ones with functional attributes) in the environment have been defined.
    * The environment is still unsealed. The following line separates out rules that have functional symbols in them
    */
  val functionalKoreRules: Set[kore.Rule] = functionalLabelRulesMap.values.flatten.toSet

  /**
    * Self-explanatory, rules that don't have functional Symbols. I just convert them to a set of Rewrite(s) in Kale.
    * The conversion follows the method used in the earlier hook, but with Kore structures instead of frontend structures.
    */
  val regularRules: Set[Rewrite] = (modules.flatMap(_.rules).toSet[kore.Rule] -- functionalKoreRules).map(StandardConverter.apply)

  /**
    * Now, before sealing the environment, I convert all Rules with functional Symbols from Kore Rules to Kale Rules.
    * Since the environment is unsealed, this should go through without a problem
    */

    val functionalLabelRewriteMap: Map[Label, Set[Rewrite]] = functionalLabelRulesMap.map({
      case (k, v) => (k, v.map(StandardConverter.apply))
    })

//  val functionalLabelRewriteMap: Map[Label, Set[Rewrite]] = functionalLabelRulesMap.mapValues({ x: Set[Rule] => x.map((y: Rule) => StandardConverter(y)) })


  /**
    * Now Since we're done with all conversions, I seal the environment.
    */
  //  val labels: Iterable[Rewrite] = functionalLabelRewriteMap.values.flatten

  env.seal()


  /**
    * Next two lines are matcher and unifier, needed for creating a Rewriter.
    */
  val substitutionApplier = SubstitutionWithContext(_)

  val unifier: MatcherOrUnifier = SingleSortedMatcher()

  /**
    * Create the rewriter, needed for setting rules in FunctionDefinedByRewritingLabel
    */
  val rewriterGenerator = Rewriter(substitutionApplier, unifier)


  /**
    * Getting all FunctionDefinedByRewritingLabel(s) from the environment
    */

  val functionDefinedByRewritingLabels: Set[Label with FunctionDefinedByRewriting] = env.labels.collect({
    case l: FunctionDefinedByRewriting => l
  })

  /**
    * Setting Rules in the Labels. This Leads to an error, which I'm not
    * not sure about.
    */
  functionDefinedByRewritingLabels.foreach(x => {
    x.setRules(functionalLabelRewriteMap.getOrElse(x, Set()))(x => rewriterGenerator(x))
  })

  val rewriter = rewriterGenerator(regularRules)

  override def step(p: Pattern, steps: Int): Pattern = rewriter(p.asInstanceOf[Term]).toList.head

  private def reconstruct(inhibitForLabel: Label)(t: Term): Term = t match {
    case Node(label, children) if label != inhibitForLabel => label(children map reconstruct(inhibitForLabel))
    case t => t
  }

  private def resolveFunctionRHS(functionRules: Map[Label, Set[Rewrite]]): Map[Label, Set[Rewrite]] =
    functionRules map { case (label, rewrites) => (label, rewrites map (rw => reconstruct(label)(rw).asInstanceOf[Rewrite])) }
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
      case Some(kore.Value(v)) => {
        env.uniqueLabels.get(s.symbol.str)
      }
      case None => None
    }
  }

}

case class IsSort(s: kore.Sort, m: kore.Module, implicit val d: kore.Definition)(implicit env: Environment) extends Named(s.str) with FunctionLabel1 {

  import org.kframework.kore.implementation.{DefaultBuilders => db}

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


/**
  * This first step of the conversion. Go through the definition, and declare all the
  * symbols. Basically, initialize the matching logic signature of the definition.
  * The environment returned is unsealed at the end of this definition.
  */

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
    import org.kframework.kore.implementation.{DefaultBuilders => db}

    implicit val iDef = d

    val allImports = m.imports


    val uniqueSymbolDecs: Seq[kore.SymbolDeclaration] = m.allSentences.collect({
      case sd@kore.SymbolDeclaration(_, s, _, _) => sd
    }).groupBy(_.symbol).mapValues(_.head).values.toSeq


    val sortDeclarations: Seq[kore.SortDeclaration] = m.allSentences.collect({
      case s@kore.SortDeclaration(_, _) => s
    })


    val assocSymbols: Seq[kore.SymbolDeclaration] = uniqueSymbolDecs.filter(isAssoc)

    val nonAssocSymbols: Seq[kore.SymbolDeclaration] = uniqueSymbolDecs.diff(assocSymbols)

    implicit val env = StandardEnvironment()


    def declareNonHookedSymbol(x: kore.SymbolDeclaration): Option[Label] = {
      if (env.uniqueLabels.contains(x.symbol.str)) {
        None
      }
      else {
        x.att.findSymbol(Encodings.function) match {
          case Some(_) => {
            if (x.symbol.str.startsWith("is")) {
              Some(IsSort(db.Sort(x.symbol.str), m, d))
            } else {
              //Functional Symbol Declaration
              x.args match {
                case Seq() => Some(FunctionDefinedByRewritingLabel0(x.symbol.str)(env))
                case Seq(_) => Some(FunctionDefinedByRewritingLabel1(x.symbol.str)(env))
                case Seq(_, _) => Some(FunctionDefinedByRewritingLabel2(x.symbol.str)(env))
                case Seq(_, _, _) => Some(FunctionDefinedByRewritingLabel3(x.symbol.str)(env))
                case Seq(_, _, _, _) => Some(FunctionDefinedByRewritingLabel4(x.symbol.str)(env))
              }
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
  def apply(d: kore.Definition, m: kore.Module): Backend = new SkalaBackend()(DefinitionToStandardEnvironment(d, m), d, m)
}


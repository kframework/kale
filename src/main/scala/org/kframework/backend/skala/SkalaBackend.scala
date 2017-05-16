package org.kframework.backend.skala

import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.standard._
import org.kframework.kale.util.Named
import org.kframework.kale.{Rewrite => _, _}
import org.kframework.kore
import org.kframework.kore.{Pattern, extended}
import org.kframework.kore.extended.{Backend, Rewriter}
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.DefaultBuilders

import scala.collection.Seq


class SkalaBackend(implicit val env: StandardEnvironment, val originalDefintion: kore.Definition) extends KoreBuilders with extended.Backend {

  import org.kframework.kore.implementation.{DefaultBuilders => db}

  override def att: kore.Attributes = originalDefintion.att

  override def modules: Seq[kore.Module] = originalDefintion.modules

  lazy val rules: Set[Rewrite] = modules.flatMap(_.rules).map({
    case kore.Rule(kore.Implies(requires, kore.And(kore.Rewrite(left, right), kore.Next(ensures))), att)
      if att.findSymbol(Encodings.macroEnc).isEmpty => {
      StandardConverter(db.Rewrite(db.And(left, db.Equals(requires, db.Top())), right)).asInstanceOf[Rewrite]
    }
    case _ => throw ConversionException("Encountered Non Uniform Rule")
  }).toSet

  lazy val substitutionApplier = SubstitutionWithContext(_)

  lazy val unifier: MatcherOrUnifier = SingleSortedMatcher()

  lazy val rewriter = Rewriter(substitutionApplier, unifier)(rules)

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

    val uniqueSymbolDecs: Seq[kore.SymbolDeclaration] = m.allSentences.collect({
      case sd@kore.SymbolDeclaration(_, s, _, _) if s != iNone => sd
    }).groupBy(_.symbol).mapValues(_.head).values.toSeq


    val sortDeclarations: Seq[kore.SortDeclaration] = m.sentences.collect({
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
              //Todo: Handle this case Better
              None
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
                // Create the AC Label
                Some(MapLabel(x.symbol.str, indexFunction, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
              } else {
                // Create the AssocLabel
                Some(new AssocWithIdListLabel(x.symbol.str, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
              }
            }
          }
        }
        case None => None
      }
    }).toSet

    env.seal()

    env
  }

  private def decodeAttributePattern(p: Option[Pattern], symbol: String): Option[String] = p match {
    case Some(kore.Application(kore.Symbol(`symbol`), Seq(kore.DomainValue(Encodings.`attributeValue`, kore.Value(v))))) => Some(v)
    case _ => None

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

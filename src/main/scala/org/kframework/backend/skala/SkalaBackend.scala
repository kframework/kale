package org.kframework.backend.skala

import org.kframework.backend.skala.backendImplicits._
import org.kframework.kale.{Sort, _}
import org.kframework.kale.builtin.{MapLabel, SetLabel, TOKEN}
import org.kframework.kale.standard.{Sort, _}
import org.kframework.kale.util.Named
import org.kframework.kore
import org.kframework.kore.extended.Backend
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.{Pattern, Rule, extended}
import org.kframework.kore.extended.implicits._

class SkalaBackend(implicit val originalDefintion: kore.Definition, val originalModule: kore.Module) extends StandardEnvironment with NoSortingMixin with KoreBuilders with extended.Backend {

  private def isAssoc(s: kore.SymbolDeclaration): Boolean = {
    s.att.is(Encodings.assoc) || s.att.is(Encodings.bag)
  }

  import org.kframework.kore.implementation.{DefaultBuilders => db}

  val module = RichModule(originalModule)(originalDefintion)

  val allImports = module.imports

  val uniqueSymbolDecs: Seq[kore.SymbolDeclaration] = module.allSentences.collect({
    case sd@kore.SymbolDeclaration(_, s, _, _) => sd
  }).groupBy(_.symbol).mapValues(_.head).values.toSeq

  val assocSymbols: Seq[kore.SymbolDeclaration] = uniqueSymbolDecs.filter(isAssoc)

  val nonAssocSymbols: Seq[kore.SymbolDeclaration] = uniqueSymbolDecs.diff(assocSymbols)

  private val subsorts = ModuleWithSubsorting(originalModule)(originalDefintion).subsorts
  private val sortsFor = ModuleWithSubsorting(originalModule)(originalDefintion).sortsFor

  /**
    * Declare All Sorts With Tokens as Token Labels
    */
  val tokenLabels: Seq[TOKEN] = module.allSentences.flatMap({
    case kore.SortDeclaration(kore.Sort(s), attributes)
      if attributes.is(Encodings.token) =>
      Some(TOKEN(Sort(s)))
    case _ => None
  })

  /**
    * General operations on Maps/Sets
    */

  def declareNonHookedSymbol(x: kore.SymbolDeclaration): Option[Label] = {
    if (uniqueLabels.contains(x.symbol.str)) {
      None
    }
    else {
      x.att.findSymbol(Encodings.function) match {
        case Some(_) => {
          if (x.symbol.str.startsWith("is")) {
            Some(IsSort(db.Sort(x.symbol.str.substring(2))))
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
            case Seq() => Some(FreeLabel0(x.symbol.str))
            case Seq(_) => Some(FreeLabel1(x.symbol.str))
            case Seq(_, _) => Some(FreeLabel2(x.symbol.str))
            case Seq(_, _, _) => Some(FreeLabel3(x.symbol.str))
            case Seq(_, _, _, _) => Some(FreeLabel4(x.symbol.str))
          }
        }
      }
    }
  }


  val hookedLabels: Set[Label] = nonAssocSymbols.flatMap(hook).toSet

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
        uniqueLabels.get(x.symbol.str) match {
          case a@Some(_) => a
          case None => {
            val index: Option[Pattern] = x.att.findSymbol(Encodings.index)
            if (x.att.findSymbol(Encodings.comm).isDefined) {
              if (index.isDefined) {
                // Both Commutative and Assoc with Index
                val indexStr: String = decodeAttributePattern(index, Encodings.index.str).get
                def indexFunction(t: Term): Term = t.children.toList(indexStr.toInt)
                // Create the AC Label with Identity Term
                Some(MapLabel(x.symbol.str, indexFunction, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
              }
              else
              // AC Without Index
                Some(SetLabel(x.symbol.str, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
            } else {
              // Create the AssocLabel with Identity Term
              Some(AssocWithIdLabel(x.symbol.str, getLabelForAtt(unitLabelValue.get).asInstanceOf[Label0]()))
            }
          }
        }
      }
      case None => None
    }
  }).toSet

  //Todo: Better Mechanism To Handle These

  val emptyKSeqLabel: FreeLabel0 = FreeLabel0("#EmptyK")
  val emptyKSeq = emptyKSeqLabel()

  val kSeq = NonAssocWithIdLabel("#KSequence", emptyKSeq)

  val kConfigVar = TOKEN(Sort("KConfigVar@BASIC-K"))

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

  override def att: kore.Attributes = originalDefintion.att

  override def modules: Seq[kore.Module] = originalModule +: RichModule(originalDefintion.modulesMap.get(originalModule.name).get)(originalDefintion).imports

  val functionLabels: collection.mutable.Map[String, Label] = uniqueLabels.filter(_._2.isInstanceOf[FunctionDefinedByRewriting])

  val functionLabelRulesMap: Map[Label, Set[Rule]] = modules.flatMap(RichModule(_)(originalDefintion).rules).collect({
    case r@kore.Rule(kore.Implies(_, kore.And(kore.Rewrite(kore.Application(kore.Symbol(l), _), _), _)), att) if functionLabels.contains(l) =>
      (label(l), r)
  }).groupBy(_._1).mapValues(_.map(_._2).toSet)

  /**
    * At this point, all symbols (including ones with functional attributes) in the environment have been defined.
    * The environment is still unsealed. The following line separates out rules that have functional symbols in them
    */
  val functionKoreRules: Set[kore.Rule] = functionLabelRulesMap.values.flatten.toSet

  /**
    * Self-explanatory, rules that don't have functional Symbols. I just convert them to a set of Rewrite(s) in Kale.
    * The conversion follows the method used in the earlier hook, but with Kore structures instead of frontend structures.
    */
  val regularRulesInitial: Set[Term] = (modules.flatMap(RichModule(_)(originalDefintion).rules).toSet[kore.Rule] -- functionKoreRules).filterNot(_.att.findSymbol(Encodings.macroEnc).isDefined).map(StandardConverter.apply)

  val regularRules = regularRulesInitial map (_ mapTD normalizedKSequenceLocalRewrite)

  /**
    * Now, before sealing the environment, convert all Rules with functional Symbols from Kore Rules to Kale Rules.
    * Since the environment is unsealed, this should go through without a problem
    */

  val functionLabelRewriteMap: Map[Label, Set[Term]] = functionLabelRulesMap.map({
    case (k, v) => (k, v.map(StandardConverter.apply))
  })

  /**
    * Functional Rules Rename Variable
    */
  val functionRulesWithRenamedVariables: Map[Label, Set[Term]] = functionLabelRewriteMap.map({
    case (k, v) => (k, v.map(renameVariables))
  })

  /**
    * Now Since we're done with all conversions, seal the environment.
    */

  seal()

  /**
    * Since setting the functional rules requires a rewriter, create the matcher and the rewriter instance.
    */
  val substitutionApplier = SubstitutionWithContext(_)

  val rewriterGenerator = Rewriter(env)

  /**
    * Following old Translation
    */
  setFunctionRules(functionRulesWithRenamedVariables)


  /**
    * Perform fixpoint Resolution after sealing the environment
    */
  val finalFunctionRules = fixpoint(resolveFunctionRHS)(functionLabelRewriteMap)

  setFunctionRules(finalFunctionRules)

  def normalizedKSequenceLocalRewrite(t: Term): Term = t match {
    case Rewrite(a@Node(label: NonAssocWithIdLabel, _), b) =>
      val label(a1, a2) = a
      label(Rewrite(a1, b), Rewrite(a2, label.identity))
    case _ => t
  }


  val rewriter = rewriterGenerator(regularRules)


  def setFunctionRules(functionRules: Map[Label, Set[Term]]): Unit = {
    labels.collect({
      case l: FunctionDefinedByRewriting => l.setRules(Or(functionRules.getOrElse(l, Set[Rewrite]())))
    })
  }


  private def reconstruct(inhibitForLabel: Label)(t: Term): Term = t match {
    case Node(label, children) if label != inhibitForLabel => {
      val changedChildren = children map reconstruct(inhibitForLabel)
      val returnVal = label(changedChildren)
      returnVal
    }
    case t => t
  }

  private def resolveFunctionRHS(functionRules: Map[Label, Set[Term]]): Map[Label, Set[Term]] =
    functionRules map {
      case (label, rewrites) => (label, rewrites map (rw => reconstruct(label)(rw)))
    }


  override def step(p: Pattern, steps: Int): Pattern = {
    val convertedK = StandardConverter(p)
    val result = rewriter(convertedK)
    result.toList.head
  }


  def execute(p: Pattern): Pattern = {
    var previousResult = StandardConverter(p)
    var result = rewriter(previousResult)
    var steps = 0
    while (result.nonEmpty && result.head != previousResult) {
      previousResult = result.head
      var stepResult = rewriter(previousResult)
      result = stepResult
      steps += 1
    }

    println(unifier)
    println(uniqueLabels.mapValues(_.getClass).mkString("\n"))
    println(rewriter.rules.mkString("\n"))

    //    println("steps: " + steps)
    //
    //    println(unifier.statsInvocations.toList.sortBy(-_._2).map({
    //      case (k, v) => k + " -> invocations: " + v
    //    }).mkString("\n"))

    if (result.isEmpty) {
      previousResult
    }
    else {
      result.head
    }
  }

  def checkSort(sort: kore.Sort, term: Term): Boolean = {
    val ss = term match {
      case v: Variable => Set(v.sort)
      case _ => sortsFor.getOrElse(DefaultBuilders.Symbol(term.label.name), Set(org.kframework.kale.standard.Sort.Top))
    }

    ss.exists(s => subsorts.<=(s, sort))
  }

  case class IsSort(s: kore.Sort) extends Named("is" + s.str) with FunctionLabel1 {
    override def f(_1: Term): Option[Term] = {
      if (!_1.isGround)
        None
      else {
        val isSubsorts: Boolean = checkSort(s, _1)
        Some(toBoolean(isSubsorts))
      }
    }
  }

  def hook(s: kore.SymbolDeclaration): Option[Label] =
    s.att.getSymbolValue(Encodings.hook) flatMap { case kore.Value(v) => uniqueLabels.get(s.symbol.str) }
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
  val token = DefaultBuilders.Symbol("token")
}

/**
  * This first step of the conversion. Go through the definition, and declare all the
  * symbols. Basically, initialize the matching logic signature of the definition.
  * The environment returned is unsealed at the end of this definition.
  */
object SkalaBackend extends extended.BackendCreator {
  def apply(d: kore.Definition, m: kore.Module): Backend = new SkalaBackend()(d, m)
}

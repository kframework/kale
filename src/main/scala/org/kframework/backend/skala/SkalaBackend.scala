package org.kframework.backend.skala

import org.kframework.backend.skala.backendImplicits._
import org.kframework.kale._
import org.kframework.kale.builtin.{MapLabel, SetLabel, TOKEN}
import org.kframework.kale.standard._
import org.kframework.kale.util.Named
import org.kframework.kore.extended.Backend
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.kore.parser.{KoreToText, TextToKore}
import org.kframework.kore.{Rewrite => _, Variable => _, _}
import org.kframework.{kale, kore}

import scala.io.Source

class SkalaBackend(implicit val originalDefintion: kore.Definition, val originalModule: kore.Module) extends StandardEnvironment with KoreBuilders with extended.Backend {

  private def isAssoc(s: kore.SymbolDeclaration): Boolean = {
    s.att.is(Encodings.assoc) || s.att.is(Encodings.bag)
  }

  import org.kframework.kore.implementation.{DefaultBuilders => db}

  val module = RichModule(originalModule)(originalDefintion)

  val allImports = module.imports

  private val allSentences = module.allSentences.toSet

  println(allSentences.filter(_.toString.contains("Name@PLUTUS-CORE-SYNTAX")).mkString("\n"))

  val uniqueSymbolDecs: Set[kore.SymbolDeclaration] = allSentences.collect({
    case sd@kore.SymbolDeclaration(_, s, _, _) => sd
  })

  private val subsorts = ModuleWithSubsorting(originalModule)(originalDefintion).subsorts
  private val sortsFor = ModuleWithSubsorting(originalModule)(originalDefintion).sortsFor

  val hooks: Map[String, Hook] = Map(
    "INT.Int" -> intHook,
    "INT.add" -> plusHook,
    "MAP.concat" -> mapHook
  )

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
        case None =>
          if (x.att.findSymbol(Encodings.token).isDefined) {
            Some(TOKEN(x.symbol.str, env.Sort(x.sort.str)))
          } else {
            // Non Functional Symbol Declaration
            x.args match {
              case Seq() => Some(FreeLabel0(x.symbol.str))
              case Seq(_) => Some(FreeLabel1(x.symbol.str))
              case Seq(_, _) => Some(FreeLabel2(x.symbol.str))
              case Seq(_, _, _) => Some(FreeLabel3(x.symbol.str))
              case Seq(_, _, _, _) => Some(FreeLabel4(x.symbol.str))
              case Seq(_, _, _, _, _) => Some(FreeLabel5(x.symbol.str))
              case Seq(_, _, _, _, _, _) => Some(FreeLabel6(x.symbol.str))
              case l: Seq[_] => Some(FreeLabelN(x.symbol.str, l.size))
            }
          }
      }
    }
  }

  def createLabelsForHookedSymbols(symbols: Set[SymbolDeclaration]): Set[SymbolDeclaration] =
    symbols filter (hook(_).isEmpty)

  fixpoint(createLabelsForHookedSymbols)(uniqueSymbolDecs)

  val unhookedLabels: Set[Label] = uniqueSymbolDecs.flatMap(declareNonHookedSymbol)

  //val nonAssocLabels = hookedLabels ++ unhookedLabels

  // Initialize Assoc Labels.
  //  val assocLabels: Set[Label] = assocSymbols.flatMap(x => {
  //    val unitLabel: Option[Pattern] = x.att.findSymbol(Encodings.unit)
  //    val unitLabelValue: Option[String] = decodeAttributePattern(unitLabel, Encodings.unit.str)
  //
  //    unitLabel match {
  //      case Some(_) => {
  //        uniqueLabels.get(x.symbol.str) match {
  //          case a@Some(_) => a
  //          case None => {
  //            val index: Option[Pattern] = x.att.findSymbol(Encodings.index)
  //            if (x.att.findSymbol(Encodings.comm).isDefined) {

  //              else
  //              // AC Without Index
  //                Some(SetLabel(x.symbol.str, label(unitLabelValue.get).asInstanceOf[Label0]()))
  //            } else {
  //              // Create the AssocLabel with Identity Term
  //              Some(AssocWithIdLabel(x.symbol.str, label(unitLabelValue.get).asInstanceOf[Label0]()))
  //            }
  //          }
  //        }
  //      }
  //      case None => None
  //    }
  //  }).toSet

  //Todo: Better Mechanism To Handle These

  val emptyKSeqLabel: FreeLabel0 = FreeLabel0("#EmptyK")
  val emptyKSeq = emptyKSeqLabel()

  val kSeq = NonAssocWithIdLabel("#KSequence", emptyKSeq)

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
  val regularRulesInitial: Set[Term] = (modules.flatMap(RichModule(_)(originalDefintion).rules).toSet[kore.Rule] -- functionKoreRules)
    .filterNot(_.att.is(Encodings.macroEnc)).map(StandardConverter.apply)
    .filter({ r =>
      And.asSet(r).exists({
        case Node(l, _) => l.name.startsWith("<") // TODO: ad-hoc check that this is actually a regular rule; replace with something better
        case Rewrite(Node(l, _), _) => l.name.startsWith("<")
      })
    })

  def localizeIsKResult(t: Term): Term = {
    val theIsKResult = t findTD {
      case p@BOOLEAN.not(Node(l: IsSort, List(v: Variable))) if v.name == Name("HOLE") => true
      case p@Node(l: IsSort, List(v: Variable)) if v.name == Name("HOLE") => true
      case _ => false
    }

    def changeLhs(f: Term => Term) = {
      def cff(t: Term): Term = t match {
        case Rewrite(l, r) => Rewrite(cff(l), r)
        case o: Term => f(o map0 cff)
      }

      cff _
    }

    theIsKResult.map({ isKResult =>
      t mapBU {
        case `isKResult` =>
          BOOLEAN.True
        case o => o
      } map0 changeLhs({
        case v@Variable((kore.Name("HOLE"), _)) =>
          And(v, Equality(isKResult, BOOLEAN.True))
        case o: Term => o
      })
    }).getOrElse(t)
  }

  val regularRules = regularRulesInitial map (_ mapTD normalizedKSequenceLocalRewrite) map localizeIsKResult

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

    //    println(unifier)
    //    println(uniqueLabels.mapValues(_.getClass).mkString("\n"))
    //    println(regularRules.mkString("\n"))

    //    println("steps: " + steps)

    //    println("rule hits: \n" + rewriter.ruleHits.mkString("\n"))

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

  def checkSort(sort: kore.Sort, term: Term): Boolean = (sort.str == "K@SORT-K") || {
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

    override val isPredicate: Option[Boolean] = Some(true)
  }

  def hook(s: kore.SymbolDeclaration): Option[Label] =
    s.att.getSymbolValue(Encodings.hook) flatMap {
      case kore.Value(hookName: String) => uniqueLabels.get(s.symbol.str) match {
        case Some(l) => Some(l)
        case None => getLabelFromHook(hookName, s.symbol.str, s.args.toList)
      }
    }

  import kale._

  type Hook = (String, List[Label], List[Term]) => kale.Label

  def intHook(labelName: String, labels: List[Label], terms: List[Term]): kale.Label = {
    assert(labels.isEmpty && terms.isEmpty)
    new ReferenceLabel[Int](labelName) {
      override protected[this] def internalInterpret(s: String): Int = s.toInt
    }
  }

  def mapHook(labelName: String, labels: List[Label], terms: List[Term]): kale.Label = {
    /*
    if (index.isDefined) {
    // Both Commutative and Assoc with Index
    val indexStr: String = decodeAttributePattern(index, Encodings.index.str).get

    def indexFunction(t: Term): Term = t.children.toList(indexStr.toInt)

    // Create the AC Label with Identity Term
    Some(MapLabel(x.symbol.str, indexFunction, label(unitLabelValue.get).asInstanceOf[Label0]()))
  }
     */

    ???
  }

  def plusHook(labelName: String, labels: List[Label], terms: List[Term]): kale.Label = {
    assert(labels.size == 1 && terms.isEmpty)
    PrimitiveFunction2[Int](labelName, labels.head.asInstanceOf[LeafLabel[Int]], _ + _)
  }

  def getLabelFromHook(hookContent: String, labelName: String, sorts: List[kore.Sort]): Option[Label] = {
    val hookName :: termsStrings = hookContent.split(",").toList
    val hook: Option[Hook] = hooks.get(hookName)
    val patterns: Seq[Pattern] = (termsStrings map Source.fromString) map new TextToKore(DefaultBuilders).parsePattern
    try {
      val terms = patterns map StandardConverter.apply toList
      val sortLabels = sorts map (_.str) flatMap uniqueLabels.get
      hook map (_ (labelName, sortLabels, terms))
    } catch {
      // TODO: replace exception with an Option return on StandardConverter.apply
      case e: NoSuchElementException => None
    }
  }

  override def sort(l: Label, children: Seq[Term]): kale.Sort = ???

  override def sort(l: Label): kale.Sort = ???

  override def isSort(sort: kore.Sort, term: Term): Boolean = checkSort(DefaultBuilders.Sort(sort.str), term)
}

//Todo: Move somewhere else
object Encodings {
  val iMainModule = DefaultBuilders.Symbol("#MainModule")
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
  val attributeValue = DefaultBuilders.Symbol("TOKEN_AttributeValue")
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

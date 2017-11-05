package org.kframework.kale.standard

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.{NameFromObject, LabelNamed, unreachable}
import org.kframework.kale.{Environment, Substitution, _}
import org.kframework.{kale, kore}
import org.roaringbitmap.{FastAggregation, RoaringBitmap}

import scala.annotation.switch

trait MatchingLogicMixin extends Mixin {
  _: Environment =>

  override val Truth: TruthLabel = standard.StandardTruthLabel()

  override val Top: Top = standard.TopInstance()
  override val Bottom: Bottom = standard.BottomInstance()

  override val And: DNFAndLabel = DNFAndLabel()
  override val Or: DNFOrLabel = DNFOrLabel()
  override val Not: NotLabel = NotLabel()
  override val Variable: StandardVariableLabel = standard.StandardVariableLabel()
  override val SymbolicVariable: StandardSymbolicVariableLabel = standard.StandardSymbolicVariableLabel()
  override val Equality: EqualityLabel = standard.StandardEqualityLabel()

  override val Exists: ExistsLabel = standard.SimpleExistsLabel()
  override val ForAll: ForAllLabel = standard.SimpleForAllLabel()
  override val Next: NextLabel = standard.SimpleNextLabel()

  override val Rewrite = StandardRewriteLabel()

  // TODO: non-ML
  val BindMatch = new BindMatchLabel()

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((t.variables map (v => (v, v.label(v.name + "!" + Math.random().toInt, v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }

  def SortedVarLeft(solver: Apply)(a: Variable, b: Term): Term =
    if (isSort(a.sort, b))
      And(Equality(a.asInstanceOf[Variable], b), b)
    else
      Bottom

  case class SortedVarRight(solver: Apply) extends Binary.F({ (a: Term, b: Variable) => SortedVarLeft(solver)(b, a) })

  case class AndTerm(solver: Apply) extends Binary.F({ (a: And, b: Term) =>
    if (a.nonPredicate == a) {
      val aNext = And.nextIsNow(a)
      val aNow = And.nowOnly(a)
      val solutionNow = solver(aNow, b)
      val solutionNext = solver(Next(aNext), b)
      And(solutionNow, solutionNext, a.predicate)
    } else {
      val solution = solver(a.nonPredicate, b)
      And(solution, a.predicate)
    }
  })

  case class TermAnd(solver: Apply) extends Binary.F({ (a: Term, b: And) =>
    if (b.nonPredicate == b) {
      val bNext = And.nextIsNow(b)
      val bNow = And.nowOnly(b)
      val solutionNow = solver(a, bNow)
      val solutionNext = solver(a, Next(bNext))
      And(solutionNow, solutionNext, b.predicate)
    } else {
      val solution = solver(a, b.nonPredicate)
      And(solution, b.predicate)
    }
  })

  // TODO: something is not quite right with FormulaLabel -- make sure it is correct
  case class OneIsFormula(solver: Apply) extends Binary.F({ (a: Term, b: Term) => And(a, b) })

  case class OrTerm(solver: Apply) extends Binary.F({ (a: Or, b: Term) => a map (solver(_, b)) })

  case class TermOr(solver: Apply) extends Binary.F({ (a: Term, b: Or) => b map (solver(a, _)) })

  case class Constants(solver: Apply) extends Binary.F({ (a: DomainValue[_], b: DomainValue[_]) => And(Truth(a.data == b.data), b) })

  case class BindMatchMatcher(solver: Apply) extends Binary.F({ (a: Node2, b: Term) =>
    val v = a._1.asInstanceOf[Variable]
    val p = a._2
    b.asOr map { bx =>
      val sol = solver(p, bx)
      sol.asOr map {
        case And.SPN(s, p, n) => And.SPN(And.substitution(s.asMap + (v -> n)), p, n)
      }
    }
  }
  )

  case class QuantifierTerm(solver: Apply) extends Binary.F({ (a: Node2, b: Term) =>
    val res = solver(a._2, b)
    res.asOr map {
      And.removeVariable(a._1.asInstanceOf[Variable], _)
    }
  })

  def NextTerm(solver: Apply) = { (a: SimpleNext, b: Term) =>
    And(a, b)
  }

  def TermNext(solver: Apply) = { (a: Term, b: SimpleNext) =>
    And(a, b)
  }


  def NextNext(solver: Apply) = { (a: SimpleNext, b: SimpleNext) =>
    Next(solver(a._1, b._1))
  }

  register(Binary.definePartialFunction({
    case (_, `Not`) => OneIsFormula
    case (`Not`, _) => OneIsFormula
    case (`And`, _) => AndTerm
    case (_, `And`) => TermAnd
    case (`Or`, _) => OrTerm
    case (_, `Or`) => TermOr
    case (`ForAll`, _) => QuantifierTerm
    case (`Exists`, _) => QuantifierTerm
    case (`Variable`, _) => SortedVarLeft
    case (`BindMatch`, _) => BindMatchMatcher
    case (`Equality`, `Equality`) => LeaveAlone
    case (`Next`, `Next`) => NextNext
    case (`Next`, _) => NextTerm
    case (_, `Next`) => TermNext
  }), Priority.high)

  register(Binary.definePartialFunction({
    case (a: DomainValueLabel[_], b: DomainValueLabel[_]) if a == b => Constants
  }))
}

trait MatchingLogicPostfixMixin extends Mixin {
  _: Environment with MatchingLogicMixin with HasMatcher =>

  case class LeftRewriteMatcher(solver: Binary.Apply) extends Binary.F({ (a: SimpleRewrite, b: Term) =>
    val m = solver(a._1, b)
    m.asOr map {
      case And.SPN(subs, predicates, _) =>
        val s = substitutionMaker(subs)
        And.SPN(subs, predicates, Next(s(a._2)))
    }
  })

  case class RightRewriteMatcher(solver: Binary.Apply) extends Binary.F({ (a: Term, b: SimpleRewrite) =>
    val m = solver(a, b._1)
    m.asOr map {
      case And.SPN(subs, predicates, _) =>
        val s = substitutionMaker(subs)
        And.SPN(subs, predicates, Next(s(b._2)))
    }
  })

  case class TruthMatcher(solver: Binary.Apply) extends Binary.F[Term, Term]({
    case (Bottom, _) => Bottom
    case (_, Bottom) => Bottom
    case (Top, Top) => Top
    case (Top, t) => t
    case (t, Top) => t
    case _ => throw new AssertionError("Use only the env.Top and env.Bottom Truth objects.")
  })

  register(Binary.definePartialFunction({
    case (`Rewrite`, _) => LeftRewriteMatcher
    case (Truth, _) => TruthMatcher
    case (_, Truth) => TruthMatcher
  }), Priority.high)

  register(Binary.definePartialFunction({
    case (_, `Rewrite`) => RightRewriteMatcher
  }), Priority.ultimate)
}


abstract class ReferenceLabel[T](val name: String)(implicit val env: Environment) extends PrimordialDomainValueLabel[T]

trait PrimordialDomainValueLabel[T] extends DomainValueLabel[T] {
  def apply(v: T): DomainValue[T] = StandardDomainValue(this, v)
}

private[standard] case class StandardDomainValue[T](label: DomainValueLabel[T], data: T) extends DomainValue[T]

private[standard] case class StandardVariableLabel()(implicit override val env: Environment)
  extends LabelNamed("#Variable") with VariableLabel with CluelessRoaring {
  def apply(nameAndSort: (kale.Name, kale.Sort)): Variable = StandardVariable(nameAndSort._1, nameAndSort._2)

  override protected[this] def internalInterpret(s: String): (kale.Name, kale.Sort) = s.split(":") match {
    case Array(name) => (Name(name), Sort("K"))
    case Array(name, sort) => (Name(name), Sort(sort))
  }

  var counter = 0

  def freshVariable(): Variable = freshVariable(Sort.K)

  def freshVariable(sort: Sort): Variable = {
    counter += 1
    this ((Name("_" + counter), sort))
  }

  override val isPredicate: Option[Boolean] = Some(false)
}

private[standard] case class StandardSymbolicVariableLabel()(implicit override val env: Environment)
  extends LabelNamed("#SymbolicVariable") with SymbolicVariableLabel with CluelessRoaring { // TODO: Is is really CluelessRoaring?
  override def apply(nameSortLabel: (kale.Name, kale.Sort, Label)): SymbolicVariable =
    StandardSymbolicVariable(nameSortLabel._1, nameSortLabel._2, nameSortLabel._3)

  override protected[this] def internalInterpret(s: String): (kale.Name, kale.Sort, Label) = ???

  override def generatedVariable(givenLabel: Label, counter: Int): SymbolicVariable = {
    this ((Name("_" + counter), Sort("K"), givenLabel))
  }

  override val isPredicate: Option[Boolean] = Some(false)
}

private[standard] case class StandardVariable(name: kale.Name, givenSort: kale.Sort)(implicit env: Environment) extends Variable with kore.Variable {
  override lazy val sort = givenSort

  val label = env.Variable
}

private[standard] case class StandardSymbolicVariable(override val name: kale.Name,
                                                      givenSort: kale.Sort,
                                                      override val givenLabel: Label)
                                                     (implicit env: Environment)
  extends SymbolicVariable with kore.Variable {

  override lazy val sort: kale.Sort = givenSort
  override val label: SymbolicVariableLabel = env.SymbolicVariable
}

private[standard] case class StandardTruthLabel()
                                               (implicit val env: Environment)
  extends NameFromObject with TruthLabel with RoaringLabelsFromTerm {

  def apply(v: Boolean) = if (v) env.Top else env.Bottom

  override val isPredicate: Option[Boolean] = Some(true)
}

trait RoaringLabelsFromTerm {
  self: Label =>

  override def requiredLabels(children: Iterable[Term]) =
    throw new AssertionError("Should not get here. Implemented by the term.")

  override def suppliedLabels(children: Iterable[Term]) =
    throw new AssertionError("Should not get here. Implemented by the term.")
}

private[standard] abstract class Truth(val data: Boolean)(implicit val env: Environment) extends kale.Truth {
  val label = env.Truth
}

private[standard] case class TopInstance()(implicit eenv: Environment) extends Truth(true) with kale.Top with CluelessRoaringTerm {
  override def get(v: Variable): Option[Term] = None

  def asMap = Map()

  override def toString: String = "⊤"

  override def apply(t: Term): Term = t

  override def filter(f: Variable => Boolean): Substitution = this
}

private[standard] case class BottomInstance()(implicit eenv: Environment) extends Truth(false) with kale.Bottom {
  override def toString: String = "⊥"

  override lazy val requiredLabels: RoaringBitmap = label.env.allLabelIds

  override lazy val suppliedLabels: RoaringBitmap = RoaringBitmap.bitmapOf()
}

private[standard] case class SimpleNextLabel()(implicit override val env: Environment) extends LabelNamed("=>_") with NextLabel with Projection1Roaring {
  def apply(t: Term) = t match {
    case env.Top => env.Top
    case _ => SimpleNext(t)
  }

  override val isPredicate: Option[Boolean] = None
}

private[standard] case class SimpleNext(_1: Term)(implicit env: Environment) extends Node1 with kore.Next {
  override val label = env.Next
  override lazy val isPredicate: Boolean = _1.isPredicate
}

private[standard] case class MatchLabel()(implicit override val env: StandardEnvironment) extends LabelNamed(":=") with EqualityLabel {

  import env._

  override def apply(_1: Term, _2: Term): Term = {
    if (env.isSealed) {
      Equality(_1, _2) match {
        case Equality(a, b) =>
          val unified = And.onlyPredicate(And.env.unify(a, b))
          unified match {
            case Equality(a, b) => new Matches(a, b)
            case _ => unified
          }
        case Top => Top
        case Bottom => Bottom
      }
    } else {
      new Matches(_1, _2)
    }
  }

  override def binding(_1: Variable, _2: Term): kale.Binding = Equality.binding(_1, _2)
}

private[kale] class Matches(val _1: Term, val _2: Term)(implicit env: StandardEnvironment) extends kale.Equals {
  val label = env.Match
}

private[standard] case class StandardEqualityLabel()(implicit override val env: Environment with MatchingLogicMixin)
  extends LabelNamed("=") with EqualityLabel {
  override def apply(_1: Term, _2: Term): Term = {
    val lhsOrElements = env.Or.asSet(_1)
    val rhsOrElements = env.Or.asSet(_2)

    env.Or(for (
      e1 <- lhsOrElements;
      e2 <- rhsOrElements) yield {
      inner(e1, e2)
    })
  }

  private def inner(_1: Term, _2: Term) = {
    if (_1 == _2)
      env.Top
    else if (_1.isGround && _2.isGround) {
      if (env.isSealed && !_1.isInstanceOf[SymbolicVariable] && !_2.isInstanceOf[SymbolicVariable]) {
        env.And.onlyPredicate(env.unify(_1, _2))
      } else
        new Equals(_1, _2)
    } else {
      val Variable = env.Variable
      _1.label match {
        case `Variable` =>
          if (_2.containsInConstructor(_1))
            env.Bottom
          else
            binding(_1.asInstanceOf[Variable], _2)
        case _ => new Equals(_1, _2)
      }
    }
  }

  override def binding(_1: Variable, _2: Term): Binding = {
    // TODO: fails when there is a harmless existential binder inside.
    // reactivate when we deal with binders better
    // assert(!_2.contains(_1))
    new Binding(_1.asInstanceOf[Variable], _2)
  }
}

private[kale] class Equals(val _1: Term, val _2: Term)(implicit env: Environment) extends kale.Equals {
  val label = env.Equality

  override def equals(other: Any): Boolean = other match {
    case that: Equals => this._1 == that._1 && this._2 == that._2
    case _ => false
  }
}


class Binding(val variable: Variable, val term: Term)(implicit val env: Environment with MatchingLogicMixin) extends Equals(variable, term) with kale.Binding {
  // TODO(Daejun): Cosmin: occur check failed due to the context variables
  // assert(!util.Util.contains(term, variable))
  assert(_1.isInstanceOf[Variable])

  def get(v: Variable): Option[Term] = if (_1 == v) Some(_2) else None

  def asMap = Map(variable -> term)

  override def toString: String = super[Equals].toString
}

private[standard] case class StandardRewriteLabel()(implicit val env: Environment) extends {
  val name = "=>"
} with RewriteLabel {
  def apply(_1: Term, _2: Term) = SimpleRewrite(_1, _2)

  override def requiredLabels(children: Iterable[Term]) = children.head.requiredLabels

  override def suppliedLabels(children: Iterable[Term]) = Roaring.suppliedBy(children)
}

case class SimpleRewrite(_1: Term, _2: Term)(implicit env: Environment) extends kale.Rewrite {
  override val label = env.Rewrite
}

private[standard] class GroundApplyRewrite(implicit env: Environment) extends LabelNamed("ApplyRewrite") with FunctionLabel2 {
  override def f(_1: Term, _2: Term): Option[Term] =
    if (_2.isGround) {
      Some(env.rewrite(_1, _2))
    } else {
      None
    }

  override val isPredicate: Option[Boolean] = Some(false)
}

private[standard] class OneResult(implicit penv: StandardEnvironment) extends LabelNamed("OneResult") with FunctionLabel1 {

  import env._

  override def f(_1: Term): Option[Term] =
    if (_1 == Bottom) {
      Some(Bottom)
    } else {
      Or.asSet(_1).headOption
    }

  override val isPredicate: Option[Boolean] = Some(false)
}

class Compose2(val name: String, functionLabel2: Label2, functionLabel1: FunctionLabel1)(implicit val env: StandardEnvironment) extends FunctionLabel2 {
  override def f(_1: Term, _2: Term): Option[Term] = {
    Some(functionLabel1(functionLabel2(_1, _2)))
  }

  override val isPredicate: Option[Boolean] = None
}

private[standard] case class DNFAndLabel()(implicit val env: Environment with MatchingLogicMixin) extends {
  val name = "∧"
} with AndLabel with RoaringLabelsFromTerm {

  import env._

  var apply2hits = 0L
  var applyOnNonOrHits = 0L
  var innerApplyHits = 0L

  @Normalizing
  override def apply(_1: Term, _2: Term): Term = {
    apply2hits += 1
    innerApply(_1, _2)
  }

  private def innerApply(_1: Term, _2: Term) = {
    innerApplyHits += 1

    if (_1 == Bottom || _2 == Bottom) {
      Bottom
    } else if (_1 == Top) {
      _2
    } else if (_2 == Top) {
      _1
    } else if (_1.label != Or && _2.label != Or) {
      applyOnNonOrs(_1, _2)
    } else {
      val disjunction = cartezianProduct(Or.asSet(_1), Or.asSet(_2))
      Or(disjunction)
    }

  }

  @Normalizing
  def applyOnNonOrs(_1: Term, _2: Term): Term = {
    applyOnNonOrHits += 1
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val And.SPN(sub1, pred1, And.nowAndNext(now1, next1)) = _1
      val And.SPN(sub2, pred2, And.nowAndNext(now2, next2)) = _2

      assertAtMostOneNonPred(now1, now2)
      assertAtMostOneNonPred(next1, next2)

      apply(sub1, sub2) match {
        case `Bottom` => Bottom
        case And.SPN(sub, pred, Top) =>
          val updatedPred = sub(Predicates(And.asSet(pred1) | And.asSet(pred2)))

          strongBottomize(updatedPred) {
            val And.SPN(newSub, And.set(other), Top) = updatedPred

            val finalSub = substitution(sub.asMap ++ newSub.asMap)

            And.SPN(
              finalSub,
              Predicates(other | And.asSet(finalSub(pred))),
              nonPredicates(Set(now1, now2, Next(next1), Next(next2)).map(finalSub)))
          }
      }
    }
  }

  private def assertAtMostOneNonPred(nonPred1: Term, nonPred2: Term) = {
    if (nonPred1 != Top && nonPred2 != Top && nonPred1 != nonPred2) {
      throw new AssertionError("Conjuncting non-predicate terms is not allowed. The terms are: \n" + nonPred1 + "\nand \n" + nonPred2)
    }
  }

  @NonNormalizing
  def apply(m: Map[Variable, Term]): Substitution = substitution(m)

  @Normalizing
  def apply(_1: Substitution, _2: Substitution): Term = substitution(_1, _2)


  def asMap(t: Substitution): Map[Variable, Term] = t match {
    case `Top` => Map[Variable, Term]()
    case b: Binding => Map(b.variable -> b.term)
    case s: MultipleBindings => s.m
  }

  object substitution {

    /**
      * normalizing
      */
    def apply(_1: Substitution, _2: Substitution): Term = {

      val merged = _1(_2)

      if (merged == Bottom)
        Bottom
      else {
        // TODO(Daejun): exhaustively apply to get a fixpoint, but be careful to guarantee termination
        // TODO: optimize to use the larger substitution as the first one
        val And.SPN(newSubs2, pred2: Term, nonPred2) = _1(_2)

        val And.SPN(applyingTheSubsOutOf2To1, pred3, Top) = newSubs2(_1)

        val m1 = asMap(applyingTheSubsOutOf2To1)
        val m2 = asMap(newSubs2)

        val newSub: Substitution = substitution(m1 ++ m2)

        And.SPN(newSub, And(pred2, pred3), nonPred2)
      }
    }

    /**
      * not-normalizing
      */
    def apply(m: Map[Variable, Term]): Substitution = m.size match {
      case 0 => Top
      case 1 => new Binding(m.head._1, m.head._2)
      case _ => new MultipleBindings(m)
    }

    def unapply(t: Term): Option[Map[Variable, Term]] = t match {
      case t: Substitution => Some(asMap(t))
      case _ => None
    }
  }

  /**
    * SON is a an acronym for substitution, other predicates, non-predicates
    * It splits the And into a substitution, non-substitution like predicates,
    * and other, a non-predicate (usually the Next from a rewrite)
    */
  @PerformanceCritical
  object SPN {
    @PerformanceCritical
    def from(t: Term): (Substitution, Term, Term) = t match {
      case Bottom => throw new AssertionError("Should not try to split Bottom")
      case s: Substitution => (s, Top, Top)
      case and: AndOfSubstitutionAndPredicates => (and.s, and.preds, Top)
      case and: Predicates => (Top, and, Top)
      case and: PredicatesAndNonPredicates =>
        val SPN(sub, pred, Top) = and.predicate
        (sub, pred, and.nonPredicate)
      case np: NonPredicates => (Top, Top, np)
      case o if o.isPredicate => (Top, o, Top)
      case o => (Top, Top, o)
    }

    @PerformanceCritical
    def unapply(t: Term): Option[(Substitution, Term, Term)] = t match {
      case Bottom => None
      case _ => Some(from(t))
    }

    @NonNormalizing
    @PerformanceCritical
    def apply(substitution: Substitution, predicates: Term, nonPredicates: Term): Term = {
      val substitutionAndPredicates = if (substitution == Top) {
        predicates
      } else if (predicates == Top) {
        substitution
      } else {
        new AndOfSubstitutionAndPredicates(substitution, predicates)
      }

      if (nonPredicates == Top) {
        substitutionAndPredicates
      } else if (substitutionAndPredicates == Top) {
        nonPredicates
      } else {
        PredicatesAndNonPredicates(substitutionAndPredicates, nonPredicates)
      }
    }
  }


  /**
    * Unwraps into a substitution and non-substitution terms
    */
  //  object substitutionAndTerms {
  //    @NonNormalizing
  //    def apply(pureSubstitution: Substitution, otherTerms: Iterable[Term]): Term = {
  //      val others = otherTerms filterNot (_ == env.Top)
  //
  //      assert(others forall { t => t == pureSubstitution(t) })
  //
  //      if (others.isEmpty) {
  //        pureSubstitution
  //      } else if (pureSubstitution == Top && others.size == 1) {
  //        others.head
  //      } else {
  //        strongBottomize(others.toSeq: _*) {
  //          val terms = if (others.size > 1) new Predicates(others.toSet) else others.head
  //          if (pureSubstitution == Top) {
  //            terms
  //          } else {
  //            new AndOfSubstitutionAndPredicates(pureSubstitution, terms)
  //          }
  //        }
  //      }
  //    }
  //
  //    def unapply(t: Term): Option[(Substitution, Iterable[Term])] = Some(asSubstitutionAndTerms(t))
  //  }

  var cartezianProductHits = 0L

  private def cartezianProduct(t1: Iterable[Term], t2: Iterable[Term]): Seq[Term] = {
    cartezianProductHits += 1

    for (e1 <- t1.toSeq;
         e2 <- t2.toSeq) yield {
      applyOnNonOrs(e1, e2)
    }
  }

  @NonNormalizing
  object nonPredicates {
    def apply(s: Set[Term]): Term = {
      val set = s - Top
      assert(set.forall(!_.isPredicate))
      set.size match {
        case 0 => Top
        case 1 => set.head
        case _ => NonPredicates(set)
      }
    }
  }


  object predicatesAndNonPredicate {
    def unapply(t: Term): Some[(Term, Term)] = t match {
      case tt: And => Some(tt.predicate, tt.nonPredicate)
      case tt if tt.isPredicate => Some(tt, Top)
      case tt if !tt.isPredicate => Some(Top, tt)
    }
  }

  /**
    * @param v   the variable to remove
    * @param and the and
    */
  @PerformanceCritical
  def removeVariable(v: Variable, and: Term): Term = and match {
    case s: Substitution => s.remove(v)
    case And.SPN(s, terms, next) => And.SPN(s.remove(v), terms, next)
  }

  def onlyPredicate(t: Term): Term = {
    t.asOr map {
      case And.SPN(s, p, _) => And.SPN(s, p, Top)
    }
  }

  // TODO: generalize to traversal?
  def nextIsNow(t: Term): Term = {
    t.asOr map {
      case And.SPN(s, p, n) =>
        And.SPN(s, p, innerNextIsNow(n))
    }
  }

  private def innerNextIsNow(n: Term) = {
    n.asAnd
      .filter(_.label == Next).asAnd
      .map {
        case Next(x) => x
      }
  }

  // TODO: generalize to traversal?
  def nowOnly(t: Term): Term = {
    t.asOr map {
      case And.SPN(s, p, n) =>
        And.SPN(s, p, innerNowOnly(n))
    }
  }

  private def innerNowOnly(n: Term) = {
    n.asAnd.filter(_.label != Next)
  }

  // TODO: generalize to traversal?
  def nextOnly(t: Term): Term = {
    t.asOr map {
      case And.SPN(s, p, n) =>
        And.SPN(s, p, innerNextOnly(n))
    }
  }

  private def innerNextOnly(n: Term) = {
    n.asAnd.filter(_.label == Next)
  }

  // TODO: generalize to traversal?
  object nowAndNext {
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case And.SPN(s, p, n) =>
        Some((And.SPN(s, p, innerNowOnly(n)), And.SPN(s, p, innerNextIsNow(n))))
    }
  }

  def onlyNonPredicate(t: Term): Term = {
    t.asOr map {
      case And.SPN(_, _, n) => n
    }
  }

  def anytimeIsNow(t: Term): Term = t.label match {
    case Next => t.asInstanceOf[Node1]._1
    case _ => t map0 anytimeIsNow
  }

  private type TheFold = Set[(Term, List[Term])]

  var cartezianProductWithNextHits = 0L

  private def cartezianProductWithNext(soFar: TheFold, task: MightBeSolved): TheFold = {
    cartezianProductWithNextHits += 1
    if (soFar.isEmpty) {
      soFar
    } else {
      soFar flatMap {
        case (solutionSoFar, nexts) =>
          val solvedTask = task match {
            case Solved(term) => term
            case Task(a, b) =>
              val sub = solutionSoFar match {
                case And.SPN(s, _, _) => s
              }
              env.unify(sub(a), sub(b))
          }
          Or.asSet(solvedTask) map {
            case And.SPN(s, p, n) =>
              (applyOnNonOrs(solutionSoFar, And.SPN(s, p, Top)), nexts :+ n)
          }
      }
    }
  }

  override def combine(originalTerm: Node)(solutions: MightBeSolved*): Term = {
    val res = solutions.foldLeft(Set((Top: Term, List[Term]())))(cartezianProductWithNext)
    Or(res map {
      case (And.SPN(s, other, _), l) => And.SPN(s, other, originalTerm.copy(l map s))
    })
  }

  override def combine(label: NodeLabel)(solutions: MightBeSolved*): Term = {
    val res = solutions.foldLeft(Set((Top: Term, List[Term]())))(cartezianProductWithNext)
    Or(res map {
      case (And.SPN(s, other, _), l) => And.SPN(s, other, label(l map s))
    })
  }
}

trait MightBeSolved

case class Task(a: Term, b: Term) extends MightBeSolved

case class Solved(t: Term) extends MightBeSolved

object Predicates {
  def apply(terms: Set[Term])(implicit env: Environment with MatchingLogicMixin): Term = {
    val s = terms - env.Top
    assert(s.forall(_.isPredicate))
    assert(s.forall(!_.isInstanceOf[Substitution]))
    s.size match {
      case 0 => env.Top
      case 1 => s.head
      case _ => new Predicates(s)
    }
  }
}

private[standard] final case class Predicates(terms: Set[Term])(implicit val env: Environment with MatchingLogicMixin) extends And with Assoc {

  import env._

  assert(terms.forall(_.isPredicate))
  assert(terms.forall(!_.isInstanceOf[Substitution]))
  assert(terms.size > 1, terms.toString())
  assert(!terms.contains(Bottom))
  assert(!terms.contains(Top))
  assert(terms.forall(_.label != Next))

  override val label = And

  override def _1: Term = terms.head

  override def _2: Term = if (terms.size == 2) terms.tail.head else new Predicates(terms.tail)

  override def equals(other: Any): Boolean = other match {
    case that: Predicates => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms

  override val predicate: Term = this
  override val nonPredicate: Term = Top
}

final case class NonPredicates(terms: Set[Term])(implicit env: MatchingLogicMixin) extends And {

  assert(terms.forall(!_.isPredicate))

  override lazy val requiredLabels: RoaringBitmap = Roaring.requiredFor(terms)

  override lazy val suppliedLabels: RoaringBitmap = Roaring.suppliedBy(terms)

  override val predicate: Term = env.Top
  override val nonPredicate = this
  override val label = env.And

  override def asSet: Set[Term] = terms

  override def _1: Term = terms.head

  override def _2: Term = if (terms.size == 2) terms.tail.head else NonPredicates(terms.tail)
}

final case class PredicatesAndNonPredicates(predicate: Term, nonPredicate: Term)(implicit env: MatchingLogicMixin) extends And {

  override lazy val requiredLabels: RoaringBitmap = nonPredicate.requiredLabels

  override lazy val suppliedLabels: RoaringBitmap = nonPredicate.suppliedLabels

  import env._

  override val label = And

  override def asSet: Set[Term] = And.asSet(predicate) | And.asSet(nonPredicate)

  override def _1: Term = predicate

  override def _2: Term = nonPredicate

  override def equals(obj: Any): Boolean = obj match {
    case that: PredicatesAndNonPredicates => that.predicate == this.predicate && that.nonPredicate == this.nonPredicate
    case _ => false
  }
}


private[kale] final class AndOfSubstitutionAndPredicates(val s: Substitution, val preds: Term)(implicit env: Environment) extends And with Assoc {

  import env._

  assert(And.asSet(preds).forall(_.isPredicate))
  assert(And.asSet(preds).forall(!_.isInstanceOf[Substitution]))
  assert(preds != Bottom)
  assert(And.asSet(preds).forall(_.label != Next))

  val label = And

  lazy val predicate: Term = preds match {
    case a: Predicates => And(s, a.predicate)
    case t if t.isPredicate => And(s, t)
    case _ => s
  }

  lazy val nonPredicate = Top

  lazy val _1: Term = s
  lazy val _2: Term = preds

  override def equals(other: Any): Boolean = other match {
    case that: AndOfSubstitutionAndPredicates => this.s == that.s && this.preds == that.preds
    case _ => false
  }

  override def asSet: Set[Term] = And.asSet(preds) | And.asSet(s)
}

private[standard] final class MultipleBindings(val m: Map[Variable, Term])(implicit val env: Environment with MatchingLogicMixin) extends And with Substitution with BinaryInfix with NotRoaringTerm {
  assert(m.size >= 2)
  assert(m.forall({ case (a, b) => a != b }))

  import env._

  val label = And
  lazy val _1 = Equality(m.head._1, m.head._2)
  lazy val _2: Substitution = And.substitution(m.tail)

  override def equals(other: Any): Boolean = other match {
    case that: MultipleBindings => m == that.m
    case _ => false
  }

  def get(v: Variable): Option[Term] = m.get(v)

  override def asMap: Map[Variable, Term] = m

  override def toString: String = super[BinaryInfix].toString

  override val predicate: Term = this
  override val nonPredicate = Top

  override def asSet: Set[Term] = m.map({ case (k, v) => Equality.binding(k, v) }).toSet

  override val boundVariables: Set[Variable] = m.keySet

  override def filter(f: Variable => Boolean): Substitution = {
    val newBindings = m.filterKeys(f)

    val res = if (newBindings.size == 1)
      Equality.binding(newBindings.head._1, newBindings.head._2)
    else
      new MultipleBindings(newBindings)

    res
  }
}

private[standard] case class DNFOrLabel()(implicit override val env: Environment)
  extends LabelNamed("∨") with OrLabel with DisjunctiveRoaring {

  import env._

  def apply(_1: Term, _2: Term): Term = {
    if (_1 == Bottom) {
      _2
    } else if (_2 == Bottom) {
      _1
    } else {
      asSet(_1) | asSet(_2) match {
        case s if s.isEmpty => Bottom
        case s if s.size == 1 => s.head
        case s => new OrWithAtLeastTwoElements(s)
      }
    }
  }

  @NonNormalizing
  def applyWithoutNormalizing(s: Set[Term]): Term = (s.size: @switch) match {
    case 0 => empty
    case 1 => s.head
    case 2 => new OrWithAtLeastTwoElements(s)
  }
}

private[this] class OrWithAtLeastTwoElements(val terms: Set[Term])(implicit env: Environment) extends Or {
  assert(terms.size > 1)

  import env._

  val label = Or

  lazy val _1: Term = terms.head
  lazy val _2 = Or(terms.tail.toSeq)

  override lazy val isPredicate: Boolean = terms.forall(_.isPredicate)

  override def equals(other: Any): Boolean = other match {
    case that: OrWithAtLeastTwoElements => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms
}

private[standard] case class SimpleForAllLabel()(implicit val e: Environment with MatchingLogicMixin) extends LabelNamed("∀") with ForAllLabel with Projection2Roaring {

  import env._

  override def apply(_1: Term, _2: Term): Term = {
    val v = _1.asInstanceOf[Variable]
    _2 match {
      case Bottom => Bottom
      case _ => SimpleForAll(v, _2)
    }
  }

  override val isPredicate: Option[Boolean] = None
}

case class SimpleForAll(v: Variable, p: Term)(implicit val env: Environment) extends Node2 with ForAll {
  val label = env.ForAll
  override lazy val isPredicate = p.isPredicate

  override def _1: Term = v

  override def _2: Term = p
}

private[standard] case class SimpleExistsLabel()(implicit val e: Environment with MatchingLogicMixin) extends LabelNamed("∃") with ExistsLabel with Projection2Roaring {

  import env._

  // 1. Bottom ... Bottom
  // 2. X -> concrete ... remove binding
  // 3. X -> symbolic ... leave in place ..... should probably remove it anyhow
  // 4. no X -> leave in place
  override def apply(_1: Term, _2: Term): Term = {
    val v = _1.asInstanceOf[Variable]
    _2 match {
      case Bottom => Bottom
      case And.SPN(s, pred, nonPred) if s.get(v).exists(_.isGround) =>
        And.SPN(s.remove(v), pred, nonPred)
      case _ =>
        SimpleExists(v, _2)
    }
  }
}

case class SimpleExists(v: Variable, p: Term)(implicit val env: Environment) extends Node2 with Exists {
  val label = env.Exists
  // TODO: this should be p.isPredicate but we're using it as a marker for contexts now
  override lazy val isPredicate = p.isPredicate

  override def _1: Term = v

  override def _2: Term = p

  override lazy val variables: Set[Variable] = p.variables.filterNot(_ == v)
}

case class Name(str: String) extends kale.Name

private[standard] class BindMatchLabel(implicit override val env: Environment) extends LabelNamed("BindMatch") with Label2 with Projection2Roaring {
  def apply(v: Term, p: Term) = FreeNode2(this, v.asInstanceOf[Variable], p)

  override val isPredicate: Option[Boolean] = Some(false)
}

package org.kframework.kale.standard

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.{NameFromObject, Named, unreachable}
import org.kframework.kale.{Substitution, _}
import org.kframework.{kale, kore}

import scala.collection.{Iterable, Seq}

trait MatchingLogicMixin extends Environment with HasMatcher with HasUnifier {
  override val Truth: TruthLabel = standard.StandardTruthLabel()

  override val Top: Top = standard.TopInstance()
  override val Bottom: Bottom = standard.BottomInstance()

  override val And: DNFAndLabel = DNFAndLabel()
  override val Or: DNFOrLabel = DNFOrLabel()
  override val Not: NotLabel = NotLabel()
  override val Variable: StandardVariableLabel = standard.StandardVariableLabel()
  override val Equality: EqualityLabel = standard.StandardEqualityLabel()

  override val Exists: ExistsLabel = standard.SimpleExistsLabel()
  override val ForAll: ForAllLabel = standard.SimpleForAllLabel()
  override val Next: NextLabel = standard.SimpleNextLabel()

  override val Rewrite = StandardRewriteLabel()

  // TODO: non-ML
  val BindMatch = new BindMatchLabel()

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((t.variables map (v => (v, v.label(Name(v.name + "!" + Math.random().toInt), v.sort)))).toMap)
    rename(t).asInstanceOf[T]
  }

  def SortedVarLeft(solver: Apply)(a: Variable, b: Term): Term =
    if (isSort(a.sort, b))
      And(Equality(a.asInstanceOf[Variable], b), Next(b))
    else
      Bottom

  case class SortedVarRight(solver: Apply) extends Binary.F({ (a: Term, b: Variable) => SortedVarLeft(solver)(b, a) })

  case class AndTerm(solver: Apply) extends Binary.F({ (a: And, b: Term) =>
    val solution = solver(a.nonPredicates.get, b)
    val fTerm = And(a.predicates, solution)
    fTerm
  })

  case class TermAnd(solver: Apply) extends Binary.F({ (a: Term, b: And) =>
    val solution = solver(a, b.nonPredicates.get)
    And(solution, b.predicates)
  })

  // TODO: something is not quite right with FormulaLabel -- make sure it is correct
  case class OneIsFormula(solver: Apply) extends Binary.F({ (a: Term, b: Term) => And(a, b) })

  case class OrTerm(solver: Apply) extends Binary.F({ (a: Or, b: Term) => a map (solver(_, b)) })

  case class TermOr(solver: Apply) extends Binary.F({ (a: Term, b: Or) => b map (solver(a, _)) })

  case class Constants(solver: Apply) extends Binary.F({ (a: DomainValue[_], b: DomainValue[_]) => And(Truth(a.data == b.data), Next(b)) })

  case class BindMatchMatcher(solver: Apply) extends Binary.F({ (a: Node2, b: Term) =>
    val v = a._1.asInstanceOf[Variable]
    val p = a._2
    b.asOr map { bx =>
      val sol = solver(p, bx)
      And(Equality(v, bx), sol)
    }
  }
  )

  case class ForAllTerm(solver: Apply) extends Binary.F({ (a: SimpleForAll, b: Term) =>
    And.removeVariable(a.v, solver(a.p, b))
  })

  override protected def makeMatcher: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (_, `Not`) => OneIsFormula
    case (`Not`, _) => OneIsFormula
    case (`And`, _) => AndTerm
    case (_, `And`) => TermAnd
    case (`Or`, _) => OrTerm
    case (_, `Or`) => TermOr
    case (`ForAll`, _) => ForAllTerm
    case (`Variable`, _) => SortedVarLeft
    case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants
    case (`BindMatch`, _) => BindMatchMatcher
  }).orElse(super.makeMatcher)

  override def makeUnifier: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (_, `Not`) => OneIsFormula
    case (`Not`, _) => OneIsFormula
    case (`And`, _) => AndTerm
    case (_, `And`) => TermAnd
    case (`Or`, _) => OrTerm
    case (_, `Or`) => TermOr
    case (`Variable`, _) => SortedVarLeft
    case (_, `Variable`) => SortedVarRight
    case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants
  }).orElse(super.makeUnifier)
}

trait MatchingLogicPostfixMixin extends Environment with MatchingLogicMixin {

  case class RewriteMatcher(solver: Binary.Apply) extends Binary.F({ (a: SimpleRewrite, b: Term) =>
    val m = solver(a._1, b)
    m.asOr map {
      case And.withNext(nonNext@And.substitutionAndTerms(subs, terms), _) =>
        val s = substitutionMaker(subs)
        And(Next(s(a._2)), nonNext)
    }
  })

  case class TruthMatcher(solver: Binary.Apply) extends Binary.F[Term, Term]({
    case (Bottom, _) => Bottom
    case (_, Bottom) => Bottom
    case (Top, Top) => Top
    case (Top, t) => Next(t)
    case (t, Top) => Next(t)
    case _ => throw new AssertionError("Use only the env.Top and env.Bottom Truth objects.")
  })

  override protected def makeMatcher: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (`Rewrite`, _) => RewriteMatcher
    case (Truth, _) => TruthMatcher
    case (_, Truth) => TruthMatcher
  }).orElse(super.makeMatcher)
}


abstract class ReferenceLabel[T](val name: String)(implicit val env: Environment) extends PrimordialDomainValueLabel[T]

trait PrimordialDomainValueLabel[T] extends DomainValueLabel[T] {
  def apply(v: T): DomainValue[T] = StandardDomainValue(this, v)
}

private[standard] case class StandardDomainValue[T](label: DomainValueLabel[T], data: T) extends DomainValue[T]

private[standard] case class StandardVariableLabel()(implicit override val env: Environment) extends Named("#Variable") with VariableLabel {
  def apply(name: String): Variable = apply((Name(name), Sort.K))

  def apply(name: String, sort: kale.Sort): Variable = apply((Name(name), sort))

  def apply(name: kale.Name): Variable = apply((name, Sort.K))

  def apply(nameAndSort: (kale.Name, kale.Sort)): Variable = StandardVariable(nameAndSort._1, nameAndSort._2)

  override protected[this] def internalInterpret(s: String): (kale.Name, kale.Sort) = s.split(":") match {
    case Array(name, sort) => (Name(name), Sort(sort))
  }

  var counter = 0

  def freshVariable() = {
    counter += 1
    this ((Name("_" + counter), Sort("K")))
  }
}

private[standard] case class StandardVariable(name: kale.Name, givenSort: kale.Sort)(implicit env: Environment) extends Variable with kore.Variable {
  override lazy val sort = givenSort

  val label = env.Variable
}

private[standard] case class StandardTruthLabel()(implicit val env: Environment) extends NameFromObject with TruthLabel {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

private[standard] abstract class Truth(val data: Boolean)(implicit val env: Environment) extends kale.Truth {
  val label = env.Truth
}

private[standard] case class TopInstance()(implicit eenv: Environment) extends Truth(true) with kale.Top {
  override def get(v: Variable): Option[Term] = None

  def asMap = Map()

  override def toString: String = "⊤"

  override def apply(t: Term): Term = t

  override def remove(v: Variable): Substitution = this
}

private[standard] case class BottomInstance()(implicit eenv: Environment) extends Truth(false) with kale.Bottom {
  override def toString: String = "⊥"
}

private[standard] case class SimpleNextLabel()(implicit override val env: Environment) extends Named("=>_") with NextLabel {
  def apply(t: Term) = SimpleNext(t)
}

private[standard] case class SimpleNext(_1: Term)(implicit env: Environment) extends Node1 with kore.Next {
  override val label = env.Next

  override val isPredicate = false
}

private[standard] case class MatchLabel()(implicit override val env: StandardEnvironment) extends Named(":=") with EqualityLabel {

  import env._

  override def apply(_1: Term, _2: Term): Term = {
    if (env.isSealed) {
      Equality(_1, _2) match {
        case Equality(a, b) =>
          val unified = And.filterOutNext(And.env.unify(a, b))
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

private[standard] case class StandardEqualityLabel()(implicit override val env: MatchingLogicMixin) extends Named("=") with EqualityLabel {
  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == _2)
      env.Top
    else if (_1.isGround && _2.isGround) {
      if (env.isSealed) {
        val env.And.withNext(p, _) = env.unify(_1, _2)
        p
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
    assert(!_2.contains(_1))
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


class Binding(val variable: Variable, val term: Term)(implicit val env: MatchingLogicMixin) extends Equals(variable, term) with kale.Binding {
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
}

case class SimpleRewrite(_1: Term, _2: Term)(implicit env: Environment) extends kale.Rewrite {
  override val label = env.Rewrite
}

private[standard] class GroundApplyRewrite(implicit env: Environment) extends Named("ApplyRewrite") with FunctionLabel2 {
  override def f(_1: Term, _2: Term): Option[Term] =
    if (_2.isGround) {
      Some(env.rewrite(_1, _2))
    } else {
      None
    }
}

private[standard] class OneResult(implicit penv: StandardEnvironment) extends Named("OneResult") with FunctionLabel1 {

  import env._

  override def f(_1: Term): Option[Term] =
    if (_1 == Bottom) {
      Some(Bottom)
    } else {
      (Or.asSet(_1).view collect {
        case t@And.withNext(_, _) => t
      }).headOption
    }
}

class Compose2(val name: String, functionLabel2: Label2, functionLabel1: FunctionLabel1)(implicit val env: StandardEnvironment) extends FunctionLabel2 {
  override def f(_1: Term, _2: Term): Option[Term] = {
    Some(functionLabel1(functionLabel2(_1, _2)))
  }
}

private[standard] case class DNFAndLabel()(implicit val env: MatchingLogicMixin) extends {
  val name = "∧"
} with AndLabel {

  import env._

  @Normalizing
  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom) {
      Bottom
    } else if (_1 == Top) {
      _2
    } else if (_2 == Top) {
      _1
    } else {
      val disjunction = cartezianProduct(Or.asSet(_1), Or.asSet(_2))
      Or(disjunction)
    }
  }

  @Normalizing
  def applyOnNonOrs(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val withNext(substitutionAndTerms(sub1, terms1), next1) = _1
      val withNext(substitutionAndTerms(sub2, terms2), next2) = _2

      assert(!(next1.isDefined && next2.isDefined))

      apply(sub1, sub2) match {
        case `Bottom` => Bottom
        case substitutionAndTerms(sub, terms) =>
          val x = apply(sub, (terms1 ++ terms2 map sub) ++ terms)
          next1.orElse(next2).map(t => And.withNext(x, sub(t))).getOrElse(x)
        case _ => unreachable()
      }
    }
  }

  @NonNormalizing
  def apply(m: Map[Variable, Term]): Substitution = substitution(m)

  @Normalizing
  def apply(_1: Substitution, _2: Substitution): Term = substitution(_1, _2)

  @NonNormalizing
  def apply(pureSubstitution: Substitution, others: Iterable[Term]): Term = {
    val negatedANot = others exists {
      case Not(n) => pureSubstitution.contains(n)
      case _ => false
    }

    if (negatedANot)
      Bottom
    else
      others.find(_.label == Next).map({
        next =>
          And.withNext(substitutionAndTerms(pureSubstitution, others.filterNot(_.label == Next)), next)
      }).getOrElse(substitutionAndTerms(pureSubstitution, others))
  }


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
      // TODO(Daejun): exhaustively apply to get a fixpoint, but be careful to guarantee termination
      // TODO: optimize to use the larger substitution as the first one
      val substitutionAndTerms(newSubs2, termsOutOfSubs2: Iterable[Term]) = _1(_2)

      val applyingTheSubsOutOf2To1 = newSubs2(_1).asInstanceOf[Substitution]

      val m1 = asMap(applyingTheSubsOutOf2To1)
      val m2 = asMap(newSubs2)

      val newSub: Substitution = substitution(m1 ++ m2)

      DNFAndLabel.this.apply(newSub, termsOutOfSubs2)
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
    * @deprecated ("Use SON instead")
    */
  @PerformanceCritical
  def asSubstitutionAndTerms(t: Term): (Substitution, Set[Term]) = t match {
    case s: Substitution => (s, Set.empty)
    case and: AndOfSubstitutionAndPredicates => (and.s, And.asSet(and.terms))
    case and: AndOfTerms => (Top, and.terms)
    case And.withNext(rest, Some(next)) =>
      val (s, terms) = asSubstitutionAndTerms(rest)
      (s, terms + next)
    case t if t.label == And => ???
    case o => (Top, Set(o))
  }

  /**
    * SON is a an acronym for substitution, other, next
    * It splits the And into a substitution, non-substitution like predicates,
    * and other, a non-predicate (usually the Next from a rewrite)
    */
  @PerformanceCritical
  object SPO {
    @PerformanceCritical
    def from(t: Term): (Substitution, Term, Term) = t match {
      case s: Substitution => (s, Top, Top)
      case and: AndOfSubstitutionAndPredicates => (and.s, and.terms, Top)
      case and: AndOfTerms => (Top, and, Top)
      case and: AndWithNext =>
        val Some((s, t, _)) = unapply(and.conjunction)
        (s, t, and.nextTerm)
      case o =>
        assert(o.label != And)
        if (o.isPredicate) {
          (Top, o, Top)
        } else {
          (Top, Top, o)
        }
    }

    @PerformanceCritical
    def unapply(t: Term): Some[(Substitution, Term, Term)] = Some(from(t))

    @NonNormalizing
    @PerformanceCritical
    def apply(substitution: Substitution, predicates: Term, other: Term): Term = {
      val substitutionAndPredicates = if (substitution == Top) {
        predicates
      } else if (predicates == Top) {
        substitution
      } else {
        new AndOfSubstitutionAndPredicates(substitution, predicates)
      }

      if (other == Top) {
        substitutionAndPredicates
      } else if (substitutionAndPredicates == Top) {
        other
      } else {
        new AndWithNext(substitutionAndPredicates, other)
      }
    }
  }


  /**
    * Unwraps into a substitution and non-substitution terms
    */
  object substitutionAndTerms {
    @NonNormalizing
    def apply(pureSubstitution: Substitution, otherTerms: Iterable[Term]): Term = {
      val others = otherTerms filterNot (_ == env.Top)

      assert(others forall { t => t == pureSubstitution(t) })

      if (others.isEmpty) {
        pureSubstitution
      } else if (pureSubstitution == Top && others.size == 1) {
        others.head
      } else {
        strongBottomize(others.toSeq: _*) {
          val terms = if (others.size > 1) new AndOfTerms(others.toSet) else others.head
          if (pureSubstitution == Top) {
            terms
          } else {
            new AndOfSubstitutionAndPredicates(pureSubstitution, terms)
          }
        }
      }
    }

    def unapply(t: Term): Option[(Substitution, Iterable[Term])] = Some(asSubstitutionAndTerms(t))
  }

  private def cartezianProduct(t1: Iterable[Term], t2: Iterable[Term]): Seq[Term] = {
    for (e1 <- t1.toSeq;
         e2 <- t2.toSeq) yield {
      applyOnNonOrs(e1, e2)
    }
  }


  object predicatesAndNonPredicate {
    def unapply(t: Term): Some[(Term, Option[Term])] = t match {
      case tt: And => Some(tt.predicates, tt.nonPredicates)
      case tt if tt.isPredicate => Some(tt, None)
      case tt if !tt.isPredicate => Some(Top, Some(tt))
    }
  }

  def onlyNext(t: Term): Term = {
    t.asOr map {
      case env.And.withNext(_, Some(n)) => n
    }
  }

  /**
    * @param v   the variable to remove
    * @param and the and
    */
  @PerformanceCritical
  def removeVariable(v: Variable, and: Term): Term = and match {
    case s: Substitution => s.remove(v)
    case And.SPO(s, terms, next) => And.SPO(s.remove(v), terms, next)
  }

  def filterOutNext(t: Term): Term = {
    t.asOr map {
      case env.And.withNext(t, _) => t
    }
  }

  def nextIsNow(t: Term): Term = strongBottomize(t) {
    t.asOr map {
      case env.And.withNext(t, Some(Next(n))) => And(t, n)
    }
  }

  object withNext {
    def apply(t: Term, next: Term): Term = {
      assert(next.label == Next)
      strongBottomize(t) {
        if (t == Top)
          next
        else
          AndWithNext(t, next)
      }
    }

    def unapply(t: Term): Some[(Term, Option[Term])] = t match {
      case standard.AndWithNext(and, next) =>
        Some(and, Some(next))
      case next if t.label == Next =>
        Some(Top, Some(next))
      case _ => Some(t, None)
    }
  }

  private object an {
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case withNext(and, Some(next)) => Some(and, next.asInstanceOf[SimpleNext]._1)
      case _ => None
    }
  }

  private type TheFold = Set[(Term, List[Term])]

  private def cartezianProductWithNext(soFar: TheFold, task: MightBeSolved): TheFold = {
    if (soFar.isEmpty) {
      soFar
    } else {
      soFar flatMap {
        case (solutionSoFar, nexts) =>
          val solvedTask = task match {
            case Solved(term) => term
            case Task(a, b) =>
              val sub = solutionSoFar match {
                case And.withNext(And.substitutionAndTerms(sub, _), _) => sub
              }
              env.unify(sub(a), sub(b))
          }
          Or.asSet(solvedTask) map {
            case an(p2, next2) =>
              (applyOnNonOrs(solutionSoFar, p2), nexts :+ next2)
          }
      }
    }
  }

  override def combine(originalTerm: Node)(solutions: MightBeSolved*): Term = {
    val res = solutions.foldLeft(Set((Top: Term, List[Term]())))(cartezianProductWithNext)
    Or(res map (_ match {
      case (other@And.substitutionAndTerms(s, _), l) => And.withNext(other, Next(originalTerm.copy(l map s)))
    }))
  }

  override def combine(label: NodeLabel)(solutions: MightBeSolved*): Term = {
    val res = solutions.foldLeft(Set((Top: Term, List[Term]())))(cartezianProductWithNext)
    Or(res map {
      case (other@And.substitutionAndTerms(s, _), l) => And.withNext(other, Next(label(l map s)))
    })
  }
}

trait MightBeSolved

case class Task(a: Term, b: Term) extends MightBeSolved

case class Solved(t: Term) extends MightBeSolved

private[standard] final class AndOfTerms(val terms: Set[Term])(implicit val env: Environment) extends And with Assoc {

  import env._

  lazy val predicates: Term = And(terms filter (_.isPredicate))

  val nonPredicates: Option[Term] = {
    val nonFormulas = terms filter (!_.isPredicate)
    if (nonFormulas.size > 1) {
      throw new NotImplementedError("only handle at most one term for now")
    }
    nonFormulas.headOption
  }

  assert(terms.size > 1, terms.toString())
  assert(!terms.contains(Bottom))
  assert(!terms.contains(Top))
  assert(terms.forall(_.label != Next))

  override val label = And
  override val assocIterable: Iterable[Term] = terms

  override def _1: Term = terms.head

  override def _2: Term = if (terms.size == 2) terms.tail.head else new AndOfTerms(terms.tail)

  override def equals(other: Any): Boolean = other match {
    case that: AndOfTerms => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms
}

final case class AndWithNext(conjunction: Term, nextTerm: Term)(implicit env: MatchingLogicMixin) extends And {

  import env._

  assert(conjunction != Bottom)

  assert(nextTerm.label == Next)

  val And.predicatesAndNonPredicate(ps, nonPredicates) = conjunction
  override val predicates: Term =
    if (nonPredicates.isDefined)
      And(ps, nextTerm)
    else
      this

  override val label = And

  override def asSet: Set[Term] = And.asSet(conjunction) + nextTerm

  override val assocIterable: Iterable[Term] = asSet

  override def _1: Term = conjunction

  override def _2: Term = nextTerm

  override def equals(obj: Any): Boolean = obj match {
    case that: AndWithNext => that.conjunction == this.conjunction && that.nextTerm == this.nextTerm
    case _ => false
  }
}


private[kale] final class AndOfSubstitutionAndPredicates(val s: Substitution, val terms: Term)(implicit env: Environment) extends And with Assoc {

  import env._

  assert(terms != Bottom)
  assert(!And.asSet(terms).exists(_.label == Next))

  val label = And

  lazy val predicates: Term = terms match {
    case a: AndOfTerms => And(s, a.predicates)
    case t if t.isPredicate => And(s, t)
    case _ => s
  }

  /**
    * TODO: this should eventually be replaced by the WithNext
    */
  lazy val nonPredicates: Option[Term] = terms match {
    case a: AndOfTerms => a.nonPredicates
    case t if !t.isPredicate => Some(t)
    case _ => None
  }

  lazy val _1: Term = s
  lazy val _2: Term = terms
  override lazy val assocIterable: Iterable[Term] = And.asIterable(s) ++ And.asIterable(terms)

  override def equals(other: Any): Boolean = other match {
    case that: AndOfSubstitutionAndPredicates => this.s == that.s && this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = And.asSet(terms) | And.asSet(s)
}

private[standard] final class MultipleBindings(val m: Map[Variable, Term])(implicit val env: MatchingLogicMixin) extends And with Substitution with BinaryInfix {
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

  override val hashCode: Int = label.hashCode

  def get(v: Variable): Option[Term] = m.get(v)

  override def asMap: Map[Variable, Term] = m

  override val assocIterable: Iterable[Term] = And.asIterable(this)

  override def toString: String = super[BinaryInfix].toString

  override val predicates: Term = this
  override val nonPredicates: Option[Term] = None

  override def asSet: Set[Term] = m.map({ case (k, v) => Equality.binding(k, v) }).toSet

  override val boundVariables: Set[Variable] = m.keySet

  override def remove(v: Variable): Substitution = {
    val newBindings = m.filterKeys(_ != v)

    val res = if (newBindings.size == 1)
      Equality.binding(newBindings.head._1, newBindings.head._2)
    else
      new MultipleBindings(newBindings)

    assert(!res.contains(v))
    res
  }
}

private[standard] case class DNFOrLabel()(implicit override val env: Environment) extends Named("∨") with OrLabel {

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
}

private[this] class OrWithAtLeastTwoElements(val terms: Set[Term])(implicit env: Environment) extends Or {
  assert(terms.size > 1)

  import env._

  val label = Or

  lazy val _1: Term = terms.head
  lazy val _2 = Or(terms.tail.toSeq)
  override val assocIterable: Iterable[Term] = terms

  override lazy val isPredicate: Boolean = terms.forall(_.isPredicate)

  override def equals(other: Any): Boolean = other match {
    case that: OrWithAtLeastTwoElements => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms
}

private[standard] case class SimpleForAllLabel()(implicit val e: MatchingLogicMixin) extends Named("∀") with ForAllLabel {

  import env._

  override def apply(_1: Term, _2: Term): Term = {
    val v = _1.asInstanceOf[Variable]
    _2 match {
      case Bottom => Bottom
      case _ => SimpleForAll(v, _2)
    }
  }
}

case class SimpleForAll(v: Variable, p: Term)(implicit val env: Environment) extends Node2 with ForAll {
  val label = env.ForAll
  override val isPredicate = true

  override def _1: Term = v

  override def _2: Term = p
}

private[standard] case class SimpleExistsLabel()(implicit val e: MatchingLogicMixin) extends Named("∃") with ExistsLabel {

  import env._

  // 1. Bottom ... Bottom
  // 2. X -> concrete ... remove binding
  // 3. X -> symbolic ... leave in place ..... should probably remove it anyhow
  // 4. no X -> leave in place
  override def apply(_1: Term, _2: Term): Term = {
    val v = _1.asInstanceOf[Variable]
    _2 match {
      case Bottom => Bottom
      case And.substitutionAndTerms(s, terms) if s.get(v).exists(_.isGround) =>
        And.substitutionAndTerms(s.remove(v), terms)
      case _ =>
        SimpleExists(v, _2)
    }
  }
}

case class SimpleExists(v: Variable, p: Term)(implicit val env: Environment) extends Node2 with Exists {
  val label = env.Exists
  override val isPredicate = true

  override def _1: Term = v

  override def _2: Term = p
}

case class Name(str: String) extends kale.Name

private[standard] class BindMatchLabel(implicit override val env: Environment) extends Named("BindMatch") with Label2 {
  def apply(v: Term, p: Term) = FreeNode2(this, v.asInstanceOf[Variable], p)
}

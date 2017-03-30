package org.kframework.kale.default

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.context.{AnywhereContextApplicationLabel, Context1ApplicationLabel}

import scala.collection._
import scala.Iterable

import org.kframework.minikore.interfaces.{pattern, tree}

case class SimpleEqualityLabel(implicit val env: CurrentEnvironment) extends Named("=") with EqualityLabel {
  override def apply(_1: Term, _2: Term): Term = env.bottomize(_1, _2) {
    if (_1 == _2)
      env.Top
    else if (_1.isGround && _2.isGround) {
      env.Bottom
    } else {
      import StaticImplicits._
      val Variable = env.Variable
      _1.label match {
        case `Variable` if !_2.contains(_1) => createBinding(_1.asInstanceOf[Variable], _2)
        case _ => new Equality(_1, _2)
      }
    }
  }

  def createBinding(_1: Variable, _2: Term) = {
    import StaticImplicits._
    assert(!_2.contains(_1))
    new Binding(_1.asInstanceOf[Variable], _2)
  }
}

private[kale] class Equality(val _1: Term, val _2: Term)(implicit env: Environment) extends Node2 with BinaryInfix with pattern.Equals {
  val label = env.Equality

  override def equals(other: Any) = other match {
    case that: Equality => this._1 == that._1 && this._2 == that._2
    case _ => false
  }
}

private[kale] class Binding(val variable: Variable, val term: Term)(implicit env: CurrentEnvironment) extends Equality(variable, term) with Substitution with BinaryInfix {
  assert(_1.isInstanceOf[Variable])

  def get(v: Variable) = if (_1 == v) Some(_2) else None

  /**
    * Inefficient -- replace with some default version of ApplySubstitution
    */
  def apply(t: Term): Term = t match {
    case `variable` => term
    case Node(l: Context1ApplicationLabel, cs) =>
      val contextVar = cs.next.asInstanceOf[Variable]
      if (variable == contextVar) {
        apply(env.And.substitution(Map(env.Hole -> cs.next))(term))
      } else {
        l(contextVar, apply(cs.next))
      }
    case Node(l, cs) => l((cs map apply).toIterable)
    case _ => t
  }

  override def toString: String = super[BinaryInfix].toString
}

case class DNFAndLabel(implicit val env: CurrentEnvironment) extends {
  val name = "∧"
} with AssocWithIdLabel with AndLabel {

  import env._

  /**
    * normalizing
    */
  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val substitutionAndTerms(sub1, terms1) = _1
      val substitutionAndTerms(sub2, terms2) = _2
      val allElements: Set[Term] = terms1.toSet ++ terms2 + sub1 + sub2
      Or(allElements map Or.asSet reduce cartezianProduct)
    }
  }

  /**
    * normalizing
    */
  override def apply(terms: Iterable[Term]): Term = {
    val disjunction = terms map Or.asSet reduce cartezianProduct
    Or(disjunction)

    //    val bindings: Map[Variable, Term] = terms.collect({ case Equality(v: Variable, t) => v -> t }).toMap
    //    val pureSubstitution = Substitution(bindings)
    //    val others: Iterable[Term] = terms.filter({ case Equality(v: Variable, t) => false; case _ => true })
    //    apply(pureSubstitution, others)
  }

  /**
    * normalizing
    */
  private def applyOnNonOrs(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val substitutionAndTerms(sub1, terms1) = _1
      val substitutionAndTerms(sub2, terms2) = _2
      apply(sub1, sub2) match {
        case `Bottom` => Bottom
        case substitutionAndTerms(sub, terms) =>
          apply(sub, (terms1 ++ terms2 map sub) ++ terms)
        case _ => unreachable()
      }
    }
  }

  /**
    * not-normalizing
    */
  def apply(m: Map[Variable, Term]) = substitution(m)

  /**
    * normalizing
    */
  def apply(_1: Substitution, _2: Substitution): Term = substitution(_1, _2)

  /**
    * not-normalizing
    */
  def apply(pureSubstitution: Substitution, others: Iterable[Term]): Term = substitutionAndTerms(pureSubstitution, others)

  def asMap(t: Substitution): Map[Variable, Term] = t match {
    case `Top` => Map[Variable, Term]()
    case b: Binding => Map(b.variable -> b.term)
    case s: SubstitutionWithMultipleBindings => s.m
  }

  object formulasAndNonFormula {
    def unapply(t: Term): Some[(Term, Option[Term])] = t match {
      case tt: And => Some(tt.formulas, tt.nonFormula)
      case tt if tt.label.isInstanceOf[FormulaLabel] => Some(tt, None)
      case tt if !tt.label.isInstanceOf[FormulaLabel] => Some(Top, Some(tt))
    }
  }

  object substitution {

    /**
      * normalizing
      */
    def apply(_1: Substitution, _2: Substitution): Term = {
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
      case _ => new SubstitutionWithMultipleBindings(m)
    }

    def unapply(t: Term): Option[Map[Variable, Term]] = t match {
      case t: Substitution => Some(asMap(t))
      case _ => None
    }
  }

  def asSubstitutionAndTerms(t: Term): (Substitution, Iterable[Term]) = t match {
    case s: Substitution => (s, Iterable.empty)
    case and: AndOfSubstitutionAndTerms => (and.s, and.terms)
    case and: AndOfTerms => (Top, and.terms)
    case o => (Top, Iterable(o))
  }

  /**
    * Unwraps into a substitution and non-substitution terms
    */
  object substitutionAndTerms {
    /**
      * not normalizing
      */
    def apply(pureSubstitution: Substitution, otherTerms: Iterable[Term]): Term = {
      val others = otherTerms filterNot (_ == env.Top)

      assert(others forall { t => t == pureSubstitution(t) })

      if (others.isEmpty) {
        pureSubstitution
      } else if (pureSubstitution == Top && others.size == 1) {
        others.head
      } else {
        val terms = if (others.size > 1) new AndOfTerms(others.toSet) else others.head
        bottomize(others.toSeq: _*) {
          if (pureSubstitution == Top) {
            terms
          } else {
            new AndOfSubstitutionAndTerms(pureSubstitution, terms)
          }
        }
      }
    }

    def unapply(t: Term): Option[(Substitution, Iterable[Term])] = Some(asSubstitutionAndTerms(t))
  }

  private def cartezianProduct(t1: Iterable[Term], t2: Iterable[Term]): Seq[Term] = {
    for (e1 <- t1.toSeq; e2 <- t2.toSeq) yield {
      applyOnNonOrs(e1, e2)
    }
  }

  override def construct(l: Iterable[Term]): Term = ???

  override val identity: Term = Top

  override def asSet(t: Term): Set[Term] = t match {
    case ac: AssocComm => ac.asSet
    case _ => Set(t)
  }
}

private[kale] final class AndOfTerms(val terms: Set[Term])(implicit env: Environment) extends And with Assoc {

  import env._

  lazy val formulas: Term = And(terms filter (_.label.isInstanceOf[FormulaLabel]))

  lazy val nonFormula: Option[Term] = {
    val nonFormulas = terms filter (!_.label.isInstanceOf[FormulaLabel])
    if (nonFormulas.size > 1) {
      throw new NotImplementedError("only handle at most one term for now")
    }
    nonFormulas.headOption
  }

  assert(terms.size > 1, terms.toString())
  assert(!terms.contains(Bottom))
  assert(!terms.contains(Top))

  override val label = And
  override val assocIterable: Iterable[Term] = terms

  override def _1: Term = terms.head

  override def _2: Term = if (terms.size == 2) terms.tail.head else new AndOfTerms(terms.tail)

  override def equals(other: Any) = other match {
    case that: AndOfTerms => this.terms == that.terms
    case _ => false
  }
}

private[kale] final class AndOfSubstitutionAndTerms(val s: Substitution, val terms: Term)(implicit env: Environment) extends And with Assoc {

  import env._

  val label = And

  lazy val formulas: Term = terms match {
    case a: AndOfTerms => a.formulas
    case t if t.label.isInstanceOf[FormulaLabel] => t
    case _ => Top
  }

  lazy val nonFormula: Option[Term] = terms match {
    case a: AndOfTerms => a.nonFormula
    case t if !t.label.isInstanceOf[FormulaLabel] => Some(t)
    case _ => None
  }

  lazy val _1: Term = s
  lazy val _2: Term = terms
  override lazy val assocIterable: Iterable[Term] = And.asList(s) ++ And.asList(terms)
}

final class SubstitutionWithMultipleBindings(val m: Map[Variable, Term])(implicit env: CurrentEnvironment) extends And with Substitution with BinaryInfix {
  assert(m.size >= 2)

  import env._

  val label = And
  lazy val _1 = Equality(m.head._1, m.head._2)
  lazy val _2 = And.substitution(m.tail)

  override def equals(other: Any): Boolean = other match {
    case that: SubstitutionWithMultipleBindings => m == that.m
    case _ => false
  }

  override val hashCode: Int = label.hashCode

  def get(v: Variable) = m.get(v)

  override val assocIterable: Iterable[Term] = And.asList(this)

  /**
    * Inefficient -- replace with some default version of ApplySubstitution
    */
  def apply(t: Term): Term = t match {
    case v: Variable => m.getOrElse(v, v)
    case Node(l: Context1ApplicationLabel, cs) =>
      val contextVar = cs.next.asInstanceOf[Variable]
      m.get(contextVar).map({ context =>
        apply(And.substitution(Map(Hole -> cs.next))(context))
      }).getOrElse(l(contextVar, apply(cs.next)))

    case Node(l, cs) => l((cs map apply).toIterable)
    case _ => t
  }

  override def toString: String = super[BinaryInfix].toString
  override val formulas: Term = this
  override val nonFormula: Option[Term] = None
}

case class DNFOrLabel(implicit val env: Environment) extends Named("∨") with OrLabel {

  import env._

  def apply(_1: Term, _2: Term): Term =
    asSet(_1) | asSet(_2) match {
      case s if s.isEmpty => Bottom
      case s if s.size == 1 => s.head
      case s => new OrWithAtLeastTwoElements(s)
    }

  def asSet(t: Term): Set[Term] = t match {
    case o: OrWithAtLeastTwoElements => o.terms
    case `Bottom` => Set()
    case o => Set(o)
  }

  override def apply(l: Iterable[Term]): Term = l.foldLeft(Bottom: Term)(apply)
}

private[this] class OrWithAtLeastTwoElements(val terms: Set[Term])(implicit env: Environment) extends Or {
  assert(terms.size > 1)

  import env._

  val label = Or

  lazy val _1 = terms.head
  lazy val _2 = Or(terms.tail.toSeq)
  override val assocIterable: Iterable[Term] = terms

  override def equals(other: Any): Boolean = other match {
    case that: OrWithAtLeastTwoElements => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms
}

class AssocWithIdListLabel(val name: String, val identity: Term)(implicit val env: Environment) extends AssocWithIdLabel {
  override def construct(l: Iterable[Term]): Term = new AssocWithIdList(this, l)
}

case class AssocWithIdList(label: AssocWithIdLabel, assocIterable: Iterable[Term]) extends Assoc {
  assert(assocIterable.forall(_ != label.identity))
  assert(assocIterable.forall(_.label != label))

  override def _1: Term = assocIterable.head

  override def _2: Term = label(assocIterable.tail)
}

case class SimpleRewriteLabel(implicit val env: Environment) extends {
  val name = "=>"
} with RewriteLabel {
  def apply(_1: Term, _2: Term) = new Rewrite(_1, _2)
}

case class Rewrite(_1: Term, _2: Term)(implicit env: Environment) extends kale.Rewrite {
  override val label = env.Rewrite
}

class InvokeLabel(implicit val env: Environment) extends NameFromObject with Label1 {
  // the rewriter is initialized after the creation of the label to break the cycle when creating the rewriter for applying functions
  var rewriter: Rewriter = null
  override def apply(obj: Term): Term = Invoke(this, obj)
}

case class Invoke(label: InvokeLabel, _1: Term) extends Node1

trait FunctionDefinedByRewriting extends FunctionLabel {
  val env: CurrentEnvironment
  private var p_rewriter: Option[Rewriter] = None

  def rewriter: Rewriter = p_rewriter.get

  //throw new AssertionError("Set rules before sealing the environment. Or at least before trying to create new terms in the sealed environment.")

  def setRules(rules: Set[_ <: Rewrite]): Unit = {
    p_rewriter = Some(Rewriter(SubstitutionApply(env), Matcher(env).defaultMatcher, env)(rules))
  }

  def tryToApply(res: Term): Option[Term] =
    if (env.isSealed && rewriter.rules.nonEmpty) {
      // do not try to execute the function before the env is sealed as it would trigger the lazy initialization fo the Rewriter,
      // and a Rewriter can only be built once the Environment is sealed
      val Bottom = rewriter.env.Bottom
      rewriter.step(res).find(t => t.label != env.And && t.label != env.Or)
    } else {
      None
    }
}

case class FunctionDefinedByRewritingLabel0(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with FunctionalLabel0 {
  def f(): Option[Term] = tryToApply(FreeNode0(this))
}

case class FunctionDefinedByRewritingLabel1(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel1 {
  def f(_1: Term): Option[Term] = tryToApply(FreeNode1(this, _1))
}

case class FunctionDefinedByRewritingLabel2(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel2 {
  def f(_1: Term, _2: Term): Option[Term] = tryToApply(FreeNode2(this, _1, _2))
}

case class FunctionDefinedByRewritingLabel3(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel3 {
  def f(_1: Term, _2: Term, _3: Term): Option[Term] = tryToApply(FreeNode3(this, _1, _2, _3))
}

case class FunctionDefinedByRewritingLabel4(name: String)(implicit val env: CurrentEnvironment) extends FunctionDefinedByRewriting with PurelyFunctionalLabel4 {
  def f(_1: Term, _2: Term, _3: Term, _4: Term): Option[Term] = tryToApply(FreeNode4(this, _1, _2, _3, _4))
}

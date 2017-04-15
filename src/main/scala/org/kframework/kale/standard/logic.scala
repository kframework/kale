package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.context.Context1ApplicationLabel
import org.kframework.kale.util.{NameFromObject, Named, unreachable}
import org.kframework.minikore.interfaces.pattern

import scala.collection.{Iterable, Map, Seq, Set}

abstract class ReferenceLabel[T](val name: String)(val env: Environment) extends PrimordialConstantLabel[T]

trait PrimordialConstantLabel[T] extends ConstantLabel[T] {
  def apply(v: T): Constant[T] = SimpleConstant(this, v)
}

case class SimpleConstant[T](label: ConstantLabel[T], value: T) extends Constant[T]

case class SimpleVariableLabel(implicit val env: Environment) extends Named("#Variable") with VariableLabel {
  def apply(name: String): Variable = apply(name, Sort.K)

  def apply(nameAndSort: (String, kale.Sort)): Variable = SimpleVariable(nameAndSort._1, nameAndSort._2)
}

case class SimpleVariable(name: String, givenSort: kale.Sort)(implicit env: Environment) extends Variable with pattern.Variable {
  override lazy val sort = givenSort

  val label = env.Variable

  // FOR KORE
  override def build(_1: pattern.Name, _2: pattern.Sort): SimpleVariable = {
    assert(_2.str == "K")
    SimpleVariable(_1, _2.asInstanceOf[kale.Sort])
  }

  override def _1: pattern.Name = name

  override def _2: pattern.Sort = pattern.Sort("K")
}

case class SimpleTruthLabel(implicit val env: Environment) extends NameFromObject with TruthLabel {
  def apply(v: Boolean) = if (v) env.Top else env.Bottom
}

abstract class Truth(val value: Boolean)(implicit val env: Environment) extends kale.Truth {
  val label = env.Truth
}

case class TopInstance(implicit eenv: Environment) extends Truth(true) with kale.Top {
  override def get(v: Variable): Option[Term] = None

  def asMap = Map()

  override def toString: String = "⊤"

  def apply(t: Term): Term = t

  // FOR KORE
  override def build(): pattern.Top = this
}

case class BottomInstance(implicit eenv: Environment) extends Truth(false) with kale.Bottom {
  override def toString: String = "⊥"

  // FOR KORE
  override def build(): pattern.Bottom = this
}


case class SimpleEqualityLabel(implicit val env: DNFEnvironment) extends Named("=") with EqualityLabel {
  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == _2)
      env.Top
    else if (_1.isGround && _2.isGround) {
      env.Bottom
    } else {
      import org.kframework.kale.util.StaticImplicits._
      val Variable = env.Variable
      _1.label match {
        case `Variable` if !_2.contains(_1) => binding(_1.asInstanceOf[Variable], _2)
        case _ => new Equality(_1, _2)
      }
    }
  }

  override def binding(_1: Variable, _2: Term): Binding = {
    import org.kframework.kale.util.StaticImplicits._
    assert(!_2.contains(_1))
    new Binding(_1.asInstanceOf[Variable], _2)
  }
}

private[kale] class Equality(val _1: Term, val _2: Term)(implicit env: Environment) extends kale.Equality {
  val label = env.Equality

  override def equals(other: Any): Boolean = other match {
    case that: Equality => this._1 == that._1 && this._2 == that._2
    case _ => false
  }
}


private[kale] class Binding(val variable: Variable, val term: Term)(implicit env: DNFEnvironment) extends Equality(variable, term) with kale.Binding {
//  import org.kframework.kale.util.StaticImplicits._
//  assert(!term.contains(variable))
  assert(_1.isInstanceOf[Variable])

  def get(v: Variable): Option[Term] = if (_1 == v) Some(_2) else None

  def asMap = Map(variable -> term)

  /**
    * Inefficient -- replace with some default version of ApplySubstitution
    */
  def apply(t: Term): Term = t match {
    case `variable` => term // occur check?

    // TODO: Cosmin: move this to .context
    case Node(l: Context1ApplicationLabel, children) =>
      val cs = children.iterator
      val contextVar = cs.next.asInstanceOf[Variable]
      if (variable == contextVar) {
        apply(env.And.substitution(Map(env.Variable("☐", Sort.K) -> cs.next))(term))
      } else {
        l(contextVar, apply(cs.next))
      }
    case Node(l, cs) =>
      val newTerms = cs map apply
      l(newTerms).updatePostProcess(this)
    case _ => t
  }

  override def toString: String = super[Equality].toString
}

case class SimpleRewriteLabel(implicit val env: Environment) extends {
  val name = "=>"
} with RewriteLabel {
  def apply(_1: Term, _2: Term) = Rewrite(_1, _2)
}

case class Rewrite(_1: Term, _2: Term)(implicit env: Environment) extends kale.Rewrite {
  override val label = env.Rewrite
}

case class DNFAndLabel(implicit val env: DNFEnvironment) extends {
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
      apply(Set(_1,_2))
    }
    /*
      val substitutionAndTerms(sub1, terms1) = _1
      val substitutionAndTerms(sub2, terms2) = _2
      val allElements: Set[Term] = terms1.toSet ++ terms2 + sub1 + sub2
      Or(allElements map Or.asSet reduce cartezianProduct)
     */
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
  def apply(m: Map[Variable, Term]): Substitution = substitution(m)

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
      case tt if tt.label.isInstanceOf[PredicateLabel] => Some(tt, None)
      case tt if !tt.label.isInstanceOf[PredicateLabel] => Some(Top, Some(tt))
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

  def asSubstitutionAndTerms(t: Term): (Substitution, Set[Term]) = t match {
    case s: Substitution => (s, Set.empty)
    case and: AndOfSubstitutionAndTerms => (and.s, And.asSet(and.terms))
    case and: AndOfTerms => (Top, and.terms)
    case o => (Top, Set(o))
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
        strongBottomize(others.toSeq: _*) {
          val terms = if (others.size > 1) new AndOfTerms(others.toSet) else others.head
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
}

private[kale] final class AndOfTerms(val terms: Set[Term])(implicit val env: Environment) extends And with Assoc {

  import env._

  lazy val formulas: Term = And(terms filter (_.label.isInstanceOf[PredicateLabel]))

  lazy val nonFormula: Option[Term] = {
    val nonFormulas = terms filter (!_.label.isInstanceOf[PredicateLabel])
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

  override def equals(other: Any): Boolean = other match {
    case that: AndOfTerms => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms
}

private[kale] final class AndOfSubstitutionAndTerms(val s: Substitution, val terms: Term)(implicit env: Environment) extends And with Assoc {

  import env._

  assert(terms != Bottom)

  val label = And

  lazy val formulas: Term = terms match {
    case a: AndOfTerms => And(s, a.formulas)
    case t if t.label.isInstanceOf[PredicateLabel] => And(s, t)
    case _ => s
  }

  lazy val nonFormula: Option[Term] = terms match {
    case a: AndOfTerms => a.nonFormula
    case t if !t.label.isInstanceOf[PredicateLabel] => Some(t)
    case _ => None
  }

  lazy val _1: Term = s
  lazy val _2: Term = terms
  override lazy val assocIterable: Iterable[Term] = And.asList(s) ++ And.asList(terms)

  override def equals(other: Any): Boolean = other match {
    case that: AndOfSubstitutionAndTerms => this.s == that.s && this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = And.asSet(terms) | And.asSet(s)
}

final class SubstitutionWithMultipleBindings(val m: Map[Variable, Term])(implicit env: DNFEnvironment) extends And with Substitution with BinaryInfix {
  assert(m.size >= 2)

  import env._

  val label = And
  lazy val _1 = Equality(m.head._1, m.head._2)
  lazy val _2: Substitution = And.substitution(m.tail)

  override def equals(other: Any): Boolean = other match {
    case that: SubstitutionWithMultipleBindings => m == that.m
    case _ => false
  }

  override val hashCode: Int = label.hashCode

  def get(v: Variable): Option[Term] = m.get(v)

  override def asMap: Map[Variable, Term] = m

  override val assocIterable: Iterable[Term] = And.asList(this)

  /**
    * Inefficient -- replace with some default version of ApplySubstitution
    */
  def apply(t: Term): Term = t match {
    case v: Variable => m.getOrElse(v, v) // occur check?
    // TODO: Cosmin: move this to .context
    case Node(l: Context1ApplicationLabel, children) =>
      val cs = children.iterator
      val contextVar = cs.next.asInstanceOf[Variable]
      m.get(contextVar).map({ context =>
        apply(And.substitution(Map(Variable("☐", Sort.K) -> cs.next))(context))
      }).getOrElse(l(contextVar, apply(cs.next)))

    case n@Node(l, cs) =>
      val newTerms = (cs map apply).toIterable
      l(newTerms).updatePostProcess(this)
    case _ => t
  }

  override def toString: String = super[BinaryInfix].toString

  override val formulas: Term = this
  override val nonFormula: Option[Term] = None

  override def asSet: Set[Term] = m.map({ case (k, v) => Equality.binding(k, v) }).toSet
}

case class DNFOrLabel(implicit val env: Environment) extends Named("∨") with OrLabel {

  import env._

  def apply(_1: Term, _2: Term): Term =
    asSet(_1) | asSet(_2) match {
      case s if s.isEmpty => Bottom
      case s if s.size == 1 => s.head
      case s => new OrWithAtLeastTwoElements(s)
    }

  override def apply(l: Iterable[Term]): Term = l.foldLeft(Bottom: Term)(apply)
}

private[this] class OrWithAtLeastTwoElements(val terms: Set[Term])(implicit env: Environment) extends Or {
  assert(terms.size > 1)

  import env._

  val label = Or

  lazy val _1: Term = terms.head
  lazy val _2 = Or(terms.tail.toSeq)
  override val assocIterable: Iterable[Term] = terms

  override def equals(other: Any): Boolean = other match {
    case that: OrWithAtLeastTwoElements => this.terms == that.terms
    case _ => false
  }

  override def asSet: Set[Term] = terms
}

// implements: X and (M = (c = X) and (M = Bot implies t) and (not(m = Bot) implies e)
class IfThenElseLabel(implicit val env: Environment) extends Named("if_then_else") with Label3 {
  def apply(c: Term, t: Term, e: Term) = {
    if (c == env.Top)
      t
    else if (c == env.Bottom)
      e
    else
      FreeNode3(this, c, t, e)
  }
}

class BindMatchLabel(implicit val env: Environment) extends Named("BindMatch") with Label2 {
  def apply(v: Term, p: Term) = FreeNode2(this, v.asInstanceOf[Variable], p)
}

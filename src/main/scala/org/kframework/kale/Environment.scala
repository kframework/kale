package org.kframework.kale

import org.kframework.kale.highcats.LiftedCatsMixin
import org.kframework.kale.standard.{BottomizeMixin, StandardEnvironment}
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.transformer.{Binary, Unary}

trait Environment extends Foundation with RoaringMixin with HasMatcher with MatchingLogicMixin with LiftedCatsMixin with BottomizeMixin

trait Foundation {
  _: Environment =>

  implicit protected val env: this.type = this

  val uniqueLabels = collection.mutable.Map[String, Label]()

  def labels: Set[Label] = if (isSealed) {
    labelSet
  } else {
    uniqueLabels.values.toSet
  }

  private lazy val labelSet = uniqueLabels.values.toSet
  private var pisSealed = false

  var _matcher: Binary.Apply = _

  def seal(): Unit = {
    pisSealed = true
  }

  def isSealed = pisSealed

  def unaryProcessingFunctions: Unary.ProcessingFunctions = Unary.processingFunctions

  val substitutionMaker: Substitution => SubstitutionApply

  final val unify: Label2 = lift("unify", {
    (a: Term, b: Term) =>
      assert(this.isSealed)
      // hack to allow us to use the DNF-specific SPN here
      val And = env.asInstanceOf[StandardEnvironment].And
      unifier(a, b)
        .asOr map {
        case And.SPN(s, p, n) =>
          val newS = s(s)
          newS match {
            case Bottom => Bottom
            case ss: Substitution =>
              val rr = ss(n)
              assert((rr.variables & ss.boundVariables) == Set())
              And.SPN(ss, ss(p), rr)
          }
      }
  })

  def unifier: Binary.Apply

  def register(label: Label): Int = {
    assert(!isSealed, "Cannot register label " + label + " because the environment is sealed")
    assert(label != null)

    if (uniqueLabels.contains(label.name))
      throw new AssertionError("Label " + label.name + " already registered. The current env is: \n" + this)

    uniqueLabels.put(label.name, label)
    uniqueLabels.size
  }

  def label(labelName: String): Label = uniqueLabels(labelName)

  lazy val labelForIndex: Map[Int, Label] = labels map { l => (l.id, l) } toMap

  override def toString = {
    "nextId: " + uniqueLabels.size + "\n" + uniqueLabels.mkString("\n")
  }

  def rewrite(rule: Term, obj: Term): Term

}

trait HasMatcher extends Mixin {
  env: Environment =>

  case class NoMatch(solver: Apply) extends Binary.F({ (a: Term, b: Term) => Bottom })

  case class LeaveAlone(solver: Apply) extends Binary.F({ (a: Term, b: Term) => And(a, b) })

  case class AssertNotPossible(solver: Apply) extends Binary.F({ (a: Term, b: Term) => throw new AssertionError("Should not try to match " + a + " with " + b) })

  private var _registeredMatchers: Map[Binary.ProcessingFunctions, Int] = collection.immutable.ListMap()

  object Priority {
    val low = 30
    val medium = 50
    val high = 80
    val ultimate = 200
  }

  def registeredMatchers = _registeredMatchers

  def registerInner(matcher: Binary.ProcessingFunctions, priority: Int) {
    _registeredMatchers = _registeredMatchers + (matcher -> priority)
  }

  def registerMatcher[Process <: Apply, A <: Term, B <: Term](f: PartialFunction[(Label, Label), Process => (A, B) => Term], priority: Int) = {
    registerInner(Binary.definePartialFunction(f), priority)
  }

  final lazy val makeMatcher: Binary.ProcessingFunctions = {
    registeredMatchers
      .groupBy(_._2)
      .mapValues(_.keySet)
      .toList
      .sortBy(-_._1)
      .map(_._2)
      .map(_.reduce(_ orElse _))
      .reduceLeft(_ orElse _)
  }
}

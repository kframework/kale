package org.kframework.kale

import org.kframework.kale.context.{AnywhereContextApplicationLabel, PatternContextApplicationLabel}

import scala.collection.mutable
import org.kframework.minikore.interfaces.pattern

trait Environment extends KORELabels {
  val uniqueLabels = mutable.Map[String, Label]()

  def labels = uniqueLabels.values.toSet

  private var pisSealed = false

  def seal(): Unit = pisSealed = true

  def isSealed = pisSealed

  def register(label: Label): Int = {
    assert(!isSealed, "The environment is sealed")
    assert(label != null)

    if (uniqueLabels.contains(label.name))
      throw new AssertionError("Label " + label.name + " already registered. The current env is: \n" + this)

    uniqueLabels.put(label.name, label)
    uniqueLabels.size
  }

  def label(labelName: String): Label = uniqueLabels(labelName)

  override def toString = {
    "nextId: " + uniqueLabels.size + "\n" + uniqueLabels.mkString("\n")
  }
}

trait KORELabels {
  // Labels
  val Variable: VariableLabel
  val And: AndLabel
  val Or: OrLabel
  val Rewrite: RewriteLabel
  val Equality: EqualityLabel
  val Truth: TruthLabel

  // Constants
  val Bottom: Truth with pattern.Bottom
  val Top: Truth with Substitution with pattern.Top
}

class CurrentEnvironment extends Environment {
  implicit private val tthis = this

  val Variable = SimpleVariableLabel()

  val Truth = SimpleTruthLabel()

  val Hole = Variable("☐")

  val Top: Truth with Substitution with pattern.Top = TopInstance()
  val Bottom: Truth with pattern.Bottom = BottomInstance()

  val Equality = SimpleEqualityLabel()
  val And = DNFAndLabel()
  val Or = DNFOrLabel()
  val Rewrite = SimpleRewriteLabel()

  val AnywhereContext = AnywhereContextApplicationLabel()
  val CAPP = PatternContextApplicationLabel("CAPP")

  val builtin = new Builtins()(this)

  def bottomize(_1: Term)(f: => Term): Term = {
    if (Bottom == _1)
      Bottom
    else
      f
  }

  def bottomize(_1: Term, _2: Term)(f: => Term): Term = {
    if (Bottom == _1 || Bottom == _2)
      Bottom
    else
      f
  }

  def bottomize(terms: Term*)(f: => Term): Term = {
    if (terms.contains(Bottom))
      Bottom
    else
      f
  }

  def renameVariables[T <: Term](t: T): T = {
    val rename = And.substitution((Util.variables(t) map (v => (v, v.label(v.name + Math.random().toInt)))).toMap)
    rename(t).asInstanceOf[T]
  }
}
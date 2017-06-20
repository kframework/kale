package org.kframework.kale

import org.kframework.kale.standard.Bottomize
import org.kframework.kale.transformer.Unary
import org.kframework.kale.transformer.Unary.ProcessingFunctions
import org.kframework.kore

import scala.collection._

trait Environment extends KORELabels with Bottomize {

  implicit protected val env: this.type = this

  trait HasEnvironment {
    val env = Environment.this
  }

  val uniqueLabels = mutable.Map[String, Label]()

  def labels = uniqueLabels.values.toSet

  private var pisSealed = false

  def seal(): Unit = pisSealed = true

  def isSealed = pisSealed

  def unaryProcessingFunctions: ProcessingFunctions = Unary.processingFunctions

  val substitutionMaker: Substitution => SubstitutionApply


  final val unify = standard.lift("unify", {
    (a: Term, b: Term) =>
      assert(this.isSealed)
      unifier(a, b)
  })

  protected val unifier: MatcherOrUnifier

  def register(label: Label): Int = {
    assert(!isSealed, "Cannot register label " + label + " because the environment is sealed")
    assert(label != null)

    if (uniqueLabels.contains(label.name))
      throw new AssertionError("Label " + label.name + " already registered. The current env is: \n" + this)

    uniqueLabels.put(label.name, label)
    uniqueLabels.size
  }

  def label(labelName: String): Label = uniqueLabels(labelName)

  def sort(l: Label, children: Seq[Term]): Sort

  def sortArgs(l: Label): Seq[Sort]

  def sortTarget(l: Label): Sort

  override def toString = {
    "nextId: " + uniqueLabels.size + "\n" + uniqueLabels.mkString("\n")
  }

  def SMTName(l: Label): String

  def isZ3Builtin(l: Label): Boolean

  implicit class WithSMTname(l: Label) {
    def smtName: String = SMTName(l)
  }

  def rewrite(rule: Term, obj: Term): Term
}

trait KORELabels {
  // Constants
  val Bottom: Truth with kore.Bottom
  val Top: Truth with Substitution with kore.Top

  // Labels
  val Variable: VariableLabel
  val And: AndLabel
  val Or: OrLabel
  val Rewrite: RewriteLabel
  val Equality: EqualityLabel
  val Truth: TruthLabel
  val Not: NotLabel
  val Next: NextLabel
}


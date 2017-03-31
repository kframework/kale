package org.kframework.kale

import org.kframework.minikore.interfaces.pattern

trait FormulaLabel

trait ConstantLabel[T] extends LeafLabel[T] {

  def apply(v: T): Constant[T]

  // FOR KORE

  def interpret(s: String): Constant[T] = this (internalInterpret(s))

  // remove this and all descendants if getting rid of Constant.build
  protected[this] def internalInterpret(s: String): T
}

trait Constant[T] extends Leaf[T] with pattern.DomainValue {
  val label: ConstantLabel[T]

  val isGround = true

  override def toString: String = value.toString

  // FOR KORE
  def build(symbol: pattern.Symbol, content: pattern.Value): pattern.DomainValue = {
    symbol.asInstanceOf[ConstantLabel[_]].interpret(content)
  }

  def _1: ConstantLabel[T] = label

  def _2: String = value.toString
}

trait VariableLabel extends LeafLabel[String] {
  def apply(v: String): Variable
}

trait Variable extends Leaf[String] {
  val label: VariableLabel
  val name: String
  lazy val value: String = name
  val isGround = false

  override def toString: String = name
}

trait TruthLabel extends LeafLabel[Boolean] with FormulaLabel

trait Truth extends Leaf[Boolean] {
  val isGround = true
}

trait Top extends Truth with Substitution with pattern.Top

trait Bottom extends Truth with pattern.Bottom


trait AndLabel extends AssocCommLabel with FormulaLabel

trait OrLabel extends AssocCommLabel with FormulaLabel

trait RewriteLabel extends Label2

trait EqualityLabel extends Label2 with FormulaLabel

trait And extends Assoc with pattern.And {
  val formulas: Term
  val nonFormula: Option[Term]
}

trait Or extends AssocComm with pattern.Or

trait Rewrite extends Node2 with BinaryInfix with pattern.Rewrite

// Substitution



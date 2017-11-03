package org.kframework.kale.symbolic

import org.kframework.kale.standard.{NotLabel, StandardEnvironment, StandardSymbolicVariable}
import org.kframework.kale.{AndLabel, EqualityLabel, FreeNode1, HasLabelPredicateFunction, IMPCommonSignature, NonPrimitiveMonoidLabel, RewriteLabel, Rewriter, SymbolicVariableLabel, Term, VariableLabel}
import org.scalatest.{FlatSpec, Matchers}

class IntegrationTest extends FlatSpec with Matchers {
  private val env : StandardEnvironment = StandardEnvironment()
  private val signature = new IMPCommonSignature()(env)
  import signature._

  private val Rewrite: RewriteLabel = env.Rewrite
  private val And: AndLabel = env.And
  private val Not: NotLabel = env.Not
  private val Equality: EqualityLabel = env.Equality

  private val INT = env.INT
  private val Variable: VariableLabel = env.Variable
  private val SymbolicVariable: SymbolicVariableLabel = env.SymbolicVariable

  private val isIntLabel: HasLabelPredicateFunction = HasLabelPredicateFunction("is" + INT.Int, INT.Int)(env)
  private val isInt = (x:Term) => FreeNode1(isIntLabel, x)

  private val SymbI = SymbolicVariable("I", INT.Int)
  private val I1 = Variable("I1")
  private val I2 = Variable("I2")
  private val S = Variable("S")
  private val R = Variable("R")
  private val kseq: NonPrimitiveMonoidLabel = env.AssocWithIdLabel("_~>_", emptyk())

  "Rewriting" should "match label predicate functions" in {


    val rules: Set[Term] = Set(
      T(k(kseq(
        Rewrite(
          plus(And(I1, isInt(I1)), And(I2, isInt(I2))),
          INT.plus(I1, I2)),
        R)), S)
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()

    val rewrite = Rewriter(env)(rules)

    val term = T(k(kseq(plus(INT.Int(1), INT.Int(2)))), state(statesMap()))

    val result = rewrite.searchStep(term)
    result should equal (T(k(kseq(INT.Int(3))), state(statesMap())))
  }

  "Rewriting" should "use functions over variables." in {
    val rules: Set[Term] = Set(
      T(k(kseq(
        Rewrite(
          plus(And(I1, isInt(I1)), And(I2, isInt(I2))),
          INT.plus(I1, I2)),
        R)), S)
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()

    val rewrite = Rewriter(env)(rules)

    val term = T(k(kseq(plus(INT.Int(1), SymbI))), state(statesMap()))

    val result = rewrite.searchStep(term)
    val v1 = SymbolicVariable.generatedVariable(INT.Int, 1)
    val equalPred = Equality(v1, INT.plus.applyBuild(INT.Int(1), SymbI))
    // result should equal (And(T(k(kseq(v1)), state(statesMap())), equalPred)
    result should equal (T(k(kseq(And(v1, equalPred))), state(statesMap())))
  }

  "Rewriting" should "evaluate complex expressions with variables." in {

    val rules: Set[Term] = Set(
      /*
      T(k(kseq(
        Rewrite(
          plus(And(I1, isInt(I1)), And(I2, isInt(I2))),
          INT.plus(I1, I2)),
        R)), S),
      */
      T(k(kseq(
        Rewrite(
          plus(And(I1, isInt(I1)), And(I2, Not(isInt(I2)))),
          kseq(I2, plus(I1, env.Hole))),
        R)), S),
      /*
      T(k(kseq(
        Rewrite(
          kseq(And(I2, isInt(I2)), plus(I1, env.Hole)),
          kseq(plus(I1, I2))),
        R)), S),
        */
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()

    val rewrite = Rewriter(env)(rules)

    val term = T(k(kseq(plus(INT.Int(1), plus(INT.Int(1), And(SymbI, isInt(SymbI)))))), state(statesMap()))
    // statesMap(

    val result = rewrite.searchStep(term)
    result should equal (And(T(k(kseq(INT.plus(INT.Int(1), SymbI))), state(statesMap())), isInt(SymbI)))
  }
}

package org.kframework.kale.symbolic

import org.kframework.kale
import org.kframework.kale.standard.{NotLabel, Sort, StandardEnvironment}
import org.kframework.kale.{AndLabel, Environment, EqualityLabel, FreeNode1, HasSortPredicateFunction, IMPCommonSignature, Label, NonPrimitiveMonoidLabel, RewriteLabel, Rewriter, Term}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.Seq

class IntegrationTest extends FlatSpec with Matchers {

  val intSort = Sort("Int")
  val aExpSort = Sort("AExp")

  trait ABitOfSortingSortingMixin extends Environment {
    def sort(l: Label, children: Seq[Term]): kale.Sort = sort(l)

    def sort(l: Label): Sort = l match {
      case INT.Int => intSort
      case INT.plus => intSort
      case plus => aExpSort
      case _ => Sort.Top
    }

    override def isSort(left: org.kframework.kore.Sort, term: Term): Boolean =
      if (left == Sort.Top) {
        true
      } else if (term.sort == left) {
        true
      } else subsort(term.sort, left)

    def subsort(sort: org.kframework.kore.Sort, parentSort: org.kframework.kore.Sort): Boolean =
      sort == intSort && parentSort == aExpSort
  }

  private val env: StandardEnvironment = new StandardEnvironment with ABitOfSortingSortingMixin
  private val signature = new IMPCommonSignature()(env)

  import signature._

  private val INT = env.INT
  private val isIntLabel: HasSortPredicateFunction = HasSortPredicateFunction("is" + INT.Int, intSort)(env)
  private val isInt = (x:Term) => FreeNode1(isIntLabel, x)

  private val Rewrite: RewriteLabel = env.Rewrite
  private val And: AndLabel = env.And
  private val Not: NotLabel = env.Not
  private val Equality: EqualityLabel = env.Equality

  private val Variable = env.Variable
  private val SymbolicVariable = env.SymbolicVariable

  private val SymbI = SymbolicVariable("SymbI", intSort)
  private val I1 = Variable("I1", intSort)
  private val I2 = Variable("I2", intSort)
  private val A1 = Variable("A1", aExpSort)
  private val A2 = Variable("A2", aExpSort)
  private val S = Variable("S")
  private val R = Variable("R")
  private val kseq: NonPrimitiveMonoidLabel = env.AssocWithIdLabel("_~>_", emptyk())

  "Rewriting" should "match label predicate functions" in {


    val rules: Set[Term] = Set(
      T(k(kseq(
        Rewrite(
          plus(I1, I2),
          INT.plus(I1, I2)),
        R)), S)
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()

    val rewrite = Rewriter(env)(rules)

    val term = T(k(kseq(plus(INT.Int(1), INT.Int(2)))), state(statesMap()))

    val result = rewrite.searchStep(term)
    result should equal(T(k(kseq(INT.Int(3))), state(statesMap())))
  }

  "Rewriting" should "use functions over variables with sorts." in {
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
    val v1 = Variable.freshVariable(intSort)
    val equalPred = Equality(v1, INT.plus.applyBuild(INT.Int(1), SymbI))
    // result should equal (And(T(k(kseq(v1)), state(statesMap())), equalPred)
//    result should equal(T(k(kseq(And(v1, equalPred))), state(statesMap())))
    result should equal(And(T(k(INT.plus(INT.Int(1), SymbI)), state(statesMap())), isInt(SymbI)))
  }

  "Rewriting" should "use functions over variables." in {
    val rules: Set[Term] = Set(
      T(k(kseq(
        Rewrite(
          plus(I1, I2),
          INT.plus(I1, I2)),
        R)), S)
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()

    val rewrite = Rewriter(env)(rules)

    val term = T(k(kseq(plus(INT.Int(1), SymbI))), state(statesMap()))

    val result = rewrite.searchStep(term)
    val v1 = Variable.freshVariable(intSort)
    val equalPred = Equality(v1, INT.plus.applyBuild(INT.Int(1), SymbI))
    // result should equal (And(T(k(kseq(v1)), state(statesMap())), equalPred)
//    result should equal(T(k(kseq(And(v1, equalPred))), state(statesMap())))
    result should equal(T(k(INT.plus(INT.Int(1), SymbI)), state(statesMap())))
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
          plus(I1, I2),
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

    val term = T(k(kseq(plus(INT.Int(1), plus(INT.Int(1), SymbI)))), state(statesMap()))
    // statesMap(

    val result = rewrite.searchStep(term)
    // all rules have an implicit forall for all variables wrapping the entire rule
    // as many variables get bounded, most of the forall dissapear
    // as the Hole is free, the forall remains and is lowered to the smallest context
    // hm, not sure if forall is correct here or we should wrap it explicitly into an exists, or maybe leave it free
    result should equal(T(k(kseq(plus(INT.Int(1), SymbI), plus(INT.Int(1), env.ForAll(env.Hole, env.Hole)))), state(statesMap())))
  }

  "Rewriting" should "evaluate complex expressions (2)." in {
    val rules: Set[Term] = Set(
      T(k(kseq(
        Rewrite(
          plus(I1, I2),
          INT.plus(I1, I2)),
        R)), S),
      T(k(kseq(
        Rewrite(
          plus(I1, And(A2, Not(isInt(A2))) ),
          kseq(A2, plus(I1, env.Hole))),
        R)), S),
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()

    val rewrite = Rewriter(env)(rules)

    val term = T(k(kseq(plus(INT.Int(1), SymbI))), state(statesMap()))

    val result = rewrite.searchStep(term)
    val v1 = SymbolicVariable.generatedVariable(intSort, 1)
    val equalPred = Equality(v1, INT.plus.applyBuild(INT.Int(1), SymbI))
    // result should equal (And(T(k(kseq(v1)), state(statesMap())), equalPred)
    result should equal(T(k(kseq(And(v1, equalPred))), state(statesMap())))
    //result should equal(T(k(INT.plus(INT.Int(1), SymbI)), state(statesMap())))
  }

}

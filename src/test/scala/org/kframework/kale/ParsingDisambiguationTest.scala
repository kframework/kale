package org.kframework.kale

import org.kframework.kale.context.pattern.PatternContextApplicationLabel
import org.kframework.kale.standard._
import org.kframework.kale.util.dsl
import org.scalatest.FreeSpec

import scala.collection._
import scala.language.implicitConversions

class ParsingDisambiguationTest extends FreeSpec {

  implicit val env = StandardEnvironment()

  import env._

  val implicits = new dsl()

  val ExpId = SimpleFreeLabel1("ExpId")
  val readPointer = SimpleFreeLabel1("readPointer")
  val mult = SimpleFreeLabel2("mult")
  val plus = SimpleFreeLabel2("plus")

  val emptyExpList = SimpleFreeLabel0("emptyExpList")
  val ExpList = SimpleFreeLabel2("ExpList")

  val ExpStmt = SimpleFreeLabel1("_;")
  val block = SimpleFreeLabel1("block")
  val typedef = SimpleFreeLabel1("typedef")

  val VarDecl = SimpleFreeLabel2("VarDecl")

  val TypeId = SimpleFreeLabel1("TypeId")

  val Pointer = SimpleFreeLabel1("Pointer")

  val emptyTypeList = SimpleFreeLabel0("emptyTypeList")
  val TypeList = SimpleFreeLabel2("TypeList")

  val emptyDeclList = SimpleFreeLabel0("emptyDeclList")
  val DeclList = SimpleFreeLabel2("DeclList")

  val emptyStmtList = SimpleFreeLabel0("emptyStmtList")
  val StmtList = SimpleFreeLabel2("StmtList")

  val amb = SimpleFreeLabel2("amb")

  //  val ANYWHERE_NOT_BLOCK = PatternContextApplicationLabel("ANYWHERE_NOT_BLOCK")

  val CX = Variable("CX")
  val CX1 = Variable("CX1")

  var counter = 0

  def _V = {
    counter += 1
    Variable("_" + counter)
  }

  val TYPEDEF_CONTEXT = PatternContextApplicationLabel("TO_DECL")

  TYPEDEF_CONTEXT.setPatterns(Or(List(
    Equality(TYPEDEF_CONTEXT(CX, Hole), Hole)
  )))

  val MULT = PatternContextApplicationLabel("MULT")

  //  MULT.setPatterns(Or(List(
  //    Equality(MULT(CX, Hole), Hole),
  //    Equality(MULT(CX, Hole), ExpStmt(MULT(CX1, Hole))),
  //    Equality(MULT(CX, Hole), ExpStmt(MULT(CX1, Hole)))
  //  ))

  //  val amb = FreeLabel2("amb")

  env.seal()

  val substitutionApplier = SubstitutionWithContext(_)

  val theAmbiguity: Term = amb(
    VarDecl(
      TypeList(TypeId('a), emptyTypeList()),
      DeclList(
        Pointer('b),
        DeclList(Pointer('c), emptyDeclList()))),
    ExpStmt(
      ExpList(
        mult(ExpId('a), ExpId('b)),
        ExpList(
          readPointer(ExpId('c)),
          emptyExpList())))
  )

  def asMult(amb: Term) = StmtList(
    block(StmtList(typedef('a), emptyStmtList())),
    ExpList(
      'traversed,
      amb)
  )

  def asDecl(amb: Term) = StmtList(
    typedef('a),
    ExpList(
      'traversed,
      amb
    )
  )

  val A = Variable("A")
  val B = Variable("B")
  val C = Variable("C")
  val DeclOrNot = Variable("DeclOrNot")
  val IsDecl = Variable("IsDecl")

  val Mult = Variable("Mult")
  val Decl = Variable("Decl")

  var anywhereCounter = 0

  def ANYWHERE(p: Term) = {
    anywhereCounter += 1
    AnywhereContext(
      Variable("ANYWHERE" + anywhereCounter), p)
  }

  "with two rules and amb" in {
    val keepVarDecl =
      ANYWHERE(
        StmtList(
          TYPEDEF_CONTEXT(CX, typedef(A)),
          ANYWHERE(
            Rewrite(amb(B, ANYWHERE(ExpId(A))), B)
          )))

    val keepMult =
      ANYWHERE(
        Rewrite(amb(ANYWHERE(TypeId(A)), B), B)
      )

    val rewriterVarDecl = Rewriter(env)(Set(moveRewriteSymbolToTop(keepVarDecl)(env)))

    println(rewriterVarDecl.searchStep(asMult(theAmbiguity)))
    println(rewriterVarDecl.searchStep(asDecl(theAmbiguity)))

    val rewriter = Rewriter(env)(Set(moveRewriteSymbolToTop(keepMult)(env)))

    println(rewriter.searchStep(asMult(theAmbiguity)))
    println(rewriter.searchStep(asDecl(theAmbiguity)))
  }
}

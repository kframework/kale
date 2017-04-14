package org.kframework.kale

import org.kframework.kale.context.pattern.PatternContextApplicationLabel
import org.kframework.kale.standard._
import org.kframework.kale.util.{Implicits, Util}
import org.scalatest.FreeSpec

import scala.collection._
import scala.language.implicitConversions

class ParsingDisambiguationTest extends FreeSpec {

  implicit val env = new StandardEnvironment

  import env._

  val implicits = new Implicits()

  val ExpId = FreeLabel1("ExpId")
  val readPointer = FreeLabel1("readPointer")
  val mult = FreeLabel2("mult")
  val plus = FreeLabel2("plus")

  val emptyExpList = FreeLabel0("emptyExpList")
  val ExpList = FreeLabel2("ExpList")

  val ExpStmt = FreeLabel1("_;")
  val block = FreeLabel1("block")
  val typedef = FreeLabel1("typedef")

  val VarDecl = FreeLabel2("VarDecl")

  val TypeId = FreeLabel1("TypeId")

  val Pointer = FreeLabel1("Pointer")

  val emptyTypeList = FreeLabel0("emptyTypeList")
  val TypeList = FreeLabel2("TypeList")

  val emptyDeclList = FreeLabel0("emptyDeclList")
  val DeclList = FreeLabel2("DeclList")

  val emptyStmtList = FreeLabel0("emptyStmtList")
  val StmtList = FreeLabel2("StmtList")

  val amb = FreeLabel2("amb")

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

  implicit val unifier = SingleSortedMatcher()

  val substitutionApplier = SubstitutionWithContext(_)

  val theAmbiguity: Term = amb(
    VarDecl(
      TypeList(TypeId(ID("a")), emptyTypeList()),
      DeclList(
        Pointer(ID("b")),
        DeclList(Pointer(ID("c")), emptyDeclList()))),
    ExpStmt(
      ExpList(
        mult(ExpId(ID("a")), ExpId(ID("b"))),
        ExpList(
          readPointer(ExpId(ID("c"))),
          emptyExpList())))
  )

  def asMult(amb: Term) = StmtList(
    block(StmtList(typedef(ID("a")), emptyStmtList())),
    ExpList(
      ID("traversed"),
      amb)
  )

  def asDecl(amb: Term) = StmtList(
    typedef(ID("a")),
    ExpList(
      ID("traversed"),
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

  "match part" ignore {
    val pattern = AnywhereContext(
      Variable("ANYWHERE0"),
      StmtList(
        BindMatch(DeclOrNot,
          IfThenElse(TYPEDEF_CONTEXT(
            CX,
            typedef(A)
          ), Equality(IsDecl, Top), Equality(IsDecl, Bottom))
        ),
        Or(
          And(Not(IsDecl),
            BindMatch(Mult, ExpList(
              mult(A, B),
              readPointer(C)
            ))
          ),
          And(IsDecl,
            BindMatch(Decl, VarDecl(
              A,
              DeclList(
                Pointer(B),
                Pointer(C))
            ))
          )
        )
      )
    )

    // as decl
    assert(unifier(pattern, asDecl(theAmbiguity))
      === And(List(Equality(A, ID("a")), Equality(B, ID("b")), Equality(C, ID("c")),
      Equality(Variable("ANYWHERE0"), Variable("ANYWHERE0_1")), Equality(IsDecl, Top), Equality(CX, Hole))))

    // as mult
    assert(unifier(pattern, asMult(theAmbiguity))
      === And(List(Equality(A, ID("a")), Equality(B, ID("b")), Equality(C, ID("c")),
      Equality(Variable("ANYWHERE0"), Variable("ANYWHERE0_1")), Equality(IsDecl, Bottom))))

  }

  var anywhereCounter = 0

  def ANYWHERE(p: Term) = {
    anywhereCounter += 1
    AnywhereContext(
      Variable("ANYWHERE" + anywhereCounter), p)
  }

  "rewrite" ignore {

    val disambRule = ANYWHERE(
      StmtList(
        Rewrite(
          BindMatch(DeclOrNot,
            IfThenElse(TYPEDEF_CONTEXT(CX, typedef(A)), Equality(IsDecl, Top), Equality(IsDecl, Bottom))),
          DeclOrNot),
        Or(
          And(Not(IsDecl),
            ANYWHERE(
              Rewrite(
                BindMatch(Mult, MULT(Variable("CxMult"), mult(A, _V))),
                Mult))),
          And(IsDecl,
            ANYWHERE(
              Rewrite(
                BindMatch(Decl, VarDecl(A, _V)),
                Decl))
          ))))

    val rewriteOnTop = Util.moveRewriteSymbolToTop(disambRule)(env)

    val rewriter = Rewriter(substitutionApplier, unifier, env)(Set(rewriteOnTop))

    println(rewriter.searchStep(asMult(theAmbiguity)))
    println(rewriter.searchStep(asDecl(theAmbiguity)))
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

    val rewriterVarDecl = Rewriter(substitutionApplier, unifier, env)(Set(Util.moveRewriteSymbolToTop(keepVarDecl)(env)))

    println(rewriterVarDecl.searchStep(asMult(theAmbiguity)))
    println(rewriterVarDecl.searchStep(asDecl(theAmbiguity)))

    val rewriter = Rewriter(substitutionApplier, unifier, env)(Set(Util.moveRewriteSymbolToTop(keepMult)(env)))

    println(rewriter.searchStep(asMult(theAmbiguity)))
    println(rewriter.searchStep(asDecl(theAmbiguity)))
  }
}

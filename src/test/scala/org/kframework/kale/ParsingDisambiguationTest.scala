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

  val dotExp = FreeLabel0("dotExp")
  val ExpList = new standard.AssocWithIdListLabel("_,_", dotExp())

  val ExpStmt = FreeLabel1("_;")
  val block = FreeLabel1("block")
  val typedef = FreeLabel1("typedef")

  val VarDecl = FreeLabel2("VarDecl")

  val TypeId = FreeLabel1("TypeId")

  val Pointer = FreeLabel1("Pointer")

  // we don't have non-empty lists for now
  val emptyTypeNeList = FreeLabel0("emptyTypeNeList")
  val TypeNeList = new standard.AssocWithIdListLabel("TypeNeList", emptyTypeNeList())

  val emptyDeclList = FreeLabel0("emptyDeclList")
  val DeclList = new standard.AssocWithIdListLabel("DeclList", emptyDeclList())

  val emptyStmtList = FreeLabel0("emptyStmtList")
  val StmtList = new standard.AssocWithIdListLabel("StmtList", emptyStmtList())

  //  val ANYWHERE_NOT_BLOCK = PatternContextApplicationLabel("ANYWHERE_NOT_BLOCK")

  val CX = Variable("CX")
  val CX1 = Variable("CX1")

  var counter = 0

  def _V = {
    counter += 1
    Variable("_" + counter)
  }

  val NO_BLOCK = PatternContextApplicationLabel("NO_BLOCK")

  NO_BLOCK.setPatterns(Or(List(
    Equality(NO_BLOCK(CX, Hole), Hole),
    Equality(NO_BLOCK(CX, Hole), Pointer(NO_BLOCK(CX1, Hole)))
//    TODO: figure out how to make it lazy (we probalby need some side condition)
//    Equality(NO_BLOCK(CX, Hole), ExpList(NO_BLOCK(CX1, Hole), _V))
  )))

  val MULT = PatternContextApplicationLabel("MULT")

  MULT.setPatterns(Or(List(
    Equality(MULT(CX, Hole), Hole)
  )))

  //  val amb = FreeLabel2("amb")

  env.seal()

  implicit val unifier = SingleSortedMatcher()

  val substitutionApplier = SubstitutionWithContext(_)

  val theAmbiguity: Term = Or(
    ExpList(
      mult(ID("a"), ID("b")),
      readPointer(ID("c"))
    ),
    VarDecl(
      ID("a"),
      DeclList(
        Pointer(ID("b")),
        Pointer(ID("c")))
    )
  )

  val asMult = StmtList(
    block(typedef(ID("a"))),
    ExpList(ID("whatever"),
      theAmbiguity)
  )

  val asDecl = StmtList(
    typedef(ID("a")),
    ExpList(
      ID("whatever"),
      theAmbiguity
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
          IfThenElse(NO_BLOCK(
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
    assert(unifier(pattern, asDecl)
      === And(List(Equality(A, ID("a")), Equality(B, ID("b")), Equality(C, ID("c")),
      Equality(Variable("ANYWHERE0"), Variable("ANYWHERE0_1")), Equality(IsDecl, Top), Equality(CX, Hole))))

    // as mult
    assert(unifier(pattern, asMult)
      === And(List(Equality(A, ID("a")), Equality(B, ID("b")), Equality(C, ID("c")),
      Equality(Variable("ANYWHERE0"), Variable("ANYWHERE0_1")), Equality(IsDecl, Bottom))))

  }

  var anywhereCounter = 0

  def ANYWHERE(p: Term) = {
    anywhereCounter += 1
    AnywhereContext(
      Variable("ANYWHERE" + anywhereCounter), p)
  }

  "rewrite" in {

    val disambRule = ANYWHERE(
      StmtList(
        Rewrite(
          BindMatch(DeclOrNot,
            IfThenElse(NO_BLOCK(CX, typedef(A)), Equality(IsDecl, Top), Equality(IsDecl, Bottom))),
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

    println(rewriter.searchStep(asMult))
    println(rewriter.searchStep(asDecl))
  }
}

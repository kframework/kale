package org.kframework.kale

import org.kframework.kale.context.PatternContextApplicationLabel
import org.kframework.kale.standard._
import org.kframework.kale.util.{Implicits, Util}
import org.scalatest.FreeSpec

import scala.collection._
import scala.language.implicitConversions

class ParsingDisambiguationTest extends FreeSpec {

  implicit val env = new CurrentEnvironment

  import env._
  import env.builtin._

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

  CAPP.setPatterns(Or(List(
    Equality(CAPP(CX, Hole), Hole)
    //    Equality(CAPP(CX, Hole), dereference(CAPP(CX1, Hole)))
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

  "small match part"

  "X + 0 => X" in {
    val asMult = StmtList(
      block(typedef(ID("a"))),
      theAmbiguity
    )

    val asDecl = StmtList(
      typedef(ID("a")),
      theAmbiguity
    )

    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")

    val isDecl = Equality.binding(Variable("AMB"), ID("isDecl"))

    val disambRule = AnywhereContext(
      Variable("ANYWHERE0"),
      StmtList(
        And(
          CAPP(
            CX,
            typedef(A)
          ), isDecl),
        Or(
          Rewrite(
            And(ExpList(
              mult(A, B),
              readPointer(C)
            ), isDecl),
            Bottom
          ),
          Rewrite(
            And(VarDecl(
              A,
              DeclList(
                Pointer(B),
                Pointer(C))
            ), Not(isDecl)),
            Bottom
          )
        )
      )
    )

    val partial = AnywhereContext(
      Variable("ANYWHERE0"),
      StmtList(
        And(
          CAPP(
            CX,
            typedef(A)
          ),
          isDecl
        ),
        Or(
          And(ExpList(
            mult(A, B),
            readPointer(C)
          ), isDecl),
          And(VarDecl(
            A,
            DeclList(
              Pointer(B),
              Pointer(C))
          ), Not(isDecl))
        )
      )
    )

    val rewriteOnTop = Util.moveRewriteSymbolToTop(disambRule)(env)

    val rewriter = Rewriter(substitutionApplier, unifier, env)(Set(rewriteOnTop))

    println("as mult:")
    println(Or.asSet(unifier(partial, asMult)).mkString("\n"))
    println()
    println("as decl:")
    println(Or.asSet(unifier(partial, asDecl)).mkString("\n"))
    println()

    println(rewriter.searchStep(asMult))
    println(rewriter.searchStep(asDecl))
  }
}

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

  val mult = FreeLabel2("mult")
  val dereference = FreeLabel1("dereference")

  val emptyExp = FreeLabel0("emptyList")
  val exps = new standard.AssocWithIdListLabel("_,_", emptyExp())

  val stmt = FreeLabel1("_;")
  val emptyStmt = FreeLabel0(";")
  val stmts = new standard.AssocWithIdListLabel("_ _", emptyStmt())

  val block = FreeLabel1("block")
  val typedef = FreeLabel1("typedef")

  val declaration = FreeLabel2("declaration")
  val pointer = FreeLabel1("pointer")

  val emptyDeclList = FreeLabel0("emptyDeclList")
  val declarationVarList = new standard.AssocWithIdListLabel("declVarList", emptyDeclList())

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
    exps(
      mult(ID("a"), ID("b")),
      dereference(ID("c"))
    ),
    declaration(
      ID("a"),
      declarationVarList(
        pointer(ID("b")),
        pointer(ID("c")))
    )
  )

  "X + 0 => X" in {
    val asMult = stmts(
      block(typedef(ID("a"))),
      theAmbiguity
    )

    val asDecl = stmts(
      typedef(ID("a")),
      theAmbiguity
    )

    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")

    val isDecl = Equality.binding(Variable("AMB"), ID("isDecl"))

    val disambRule = AnywhereContext(
      Variable("ANYWHERE0"),
      stmts(
        And(
          CAPP(
            CX,
            typedef(A)
          ), isDecl),
        Or(
          Rewrite(
            And(exps(
              mult(A, B),
              dereference(C)
            ), isDecl),
            Bottom
          ),
          Rewrite(
            And(declaration(
              A,
              declarationVarList(
                pointer(B),
                pointer(C))
            ), Not(isDecl)),
            Bottom
          )
        )
      )
    )

    val partial = AnywhereContext(
      Variable("ANYWHERE0"),
      stmts(
        And(
          CAPP(
            CX,
            typedef(A)
          ),
          isDecl
        ),
        Or(
          And(exps(
            mult(A, B),
            dereference(C)
          ), isDecl),
          And(declaration(
            A,
            declarationVarList(
              pointer(B),
              pointer(C))
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

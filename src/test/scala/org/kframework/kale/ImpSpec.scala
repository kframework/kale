package org.kframework.kale

import org.kframework.kale.standard._
import org.kframework.kale.util.dsl
import org.scalatest.FreeSpec

/*
// Copyright (c) 2014-2015 K Team. All Rights Reserved.
requires "domains.k"

module IMP-CORE-SYNTAX
  imports EMPTY-ID
  imports INT-SYNTAX
  imports BOOL-SYNTAX

  syntax AExp  ::= Int | Id
                 | AExp "/" AExp              [left, strict]
                 > AExp "+" AExp              [left, strict]
                 | "(" AExp ")"               [bracket]
  syntax BExp  ::= Bool
                 | AExp "<=" AExp             [seqstrict, latex({#1}\leq{#2})]
                 | "!" BExp                   [strict]
                 > BExp "&&" BExp             [left, strict(1)]
                 | "(" BExp ")"               [bracket]
  syntax Block ::= "{" "}"
                 | "{" Stmt "}"
  syntax Stmt  ::= Block
                 | Id "=" AExp ";"            [strict(2)]
                 | "if" "(" BExp ")"
                   Block "else" Block         [strict(1)]
                 | "while" "(" BExp ")" Block
                 > Stmt Stmt                  [left]
  syntax Pgm ::= "int" Ids ";" Stmt
  syntax Ids ::= List{Id,","}
endmodule

module IMP-SYNTAX
  imports ID
  imports IMP-CORE-SYNTAX
endmodule

module IMP
  imports IMP-CORE-SYNTAX
  imports MAP
  imports INT
  syntax KResult ::= Int | Bool

  configuration <T color="yellow">
                  <k color="green"> $PGM:Pgm:K </k>
                  <state color="red"> .Map </state>
                </T>

// AExp
  rule <k> X:Id => I ...</k> <state>... X |-> I ...</state>
  rule I1:Int / I2:Int => I1 /Int I2  when I2 =/=Int 0
  rule I1:Int + I2:Int => I1 +Int I2
// BExp
  rule I1:Int <= I2:Int => I1 <=Int I2
  rule ! T:Bool => notBool T
  rule true && B => B
  rule false && _ => false
// Block
  rule {} => .K   [structural]
  rule {S} => S  [structural]
// Stmt
  rule <k> X = I:Int; => .K ...</k> <state>... X |-> `_ => I` ...</state>
  rule S1::Stmt S2::Stmt => S1 ~> S2  [structural]
  rule if (true)  S else _ => S
  rule if (false) _ else S => S
  rule while (B) S => if (B) {S while (B) S} else {}  [structural]
// Pgm
  rule <k> int `X,Xs => Xs`;_ </k> <state> Rho:Map `.Map => X|->0` </state>
    when notBool `X in keys(Rho)`
  rule int .Ids; S => S  [structural]
endmodule

 */


object IMP {
  implicit val env = StandardEnvironment()

  val signature = new IMPCommonSignature()

  import signature._
  import env._

  val ints = AssocWithIdLabel("_,_", emptyIntList())
  val kseq = AssocWithIdLabel("_~>_", emptyk())

  case class isSort(label: LeafLabel[_])(implicit val env: Environment) extends {
    val name: String = "is" + label.name
  } with FunctionLabel1 {
    def f(_1: Term): Option[Term] = Some(Truth(_1.label == label))
  }

  val isInt = isSort(INT.Int)

  //  // AExp
  //  rule <k> X:Id => I ...</k> <state>... X |-> I ...</state>
  //  rule I1:Int / I2:Int => I1 /Int I2  when I2 =/=Int 0
  //  rule I1:Int + I2:Int => I1 +Int I2
  //    // BExp
  //    rule I1:Int <= I2:Int => I1 <=Int I2
  //    rule ! T:Bool => notBool T
  //    rule true && B => B
  //  rule false && _ => false
  //  // Block
  //  rule {} => .K   [structural]
  //  rule {S} => S  [structural]
  //  // Stmt
  //  rule <k> X = I:Int; => .K ...</k> <state>... X |-> `_ => I` ...</state>
  //  rule S1::Stmt S2::Stmt => S1 ~> S2  [structural]
  //  rule if (true)  S else _ => S
  //  rule if (false) _ else S => S
  //  rule while (B) S => if (B) {S while (B) S} else {}  [structural]
  //  // Pgm
  //  rule <k> int `X,Xs => Xs`;_ </k> <state> Rho:Map `.Map => X|->0` </state>
  //  when notBool `X in keys(Rho)`
  //  rule int .Ids; S => S  [structural]


  val X = Variable("X")
  val I = Variable("I")
  val I1 = Variable("I1")
  val I2 = Variable("I2")
  val S = Variable("S")
  val SO = Variable("SO")
  val R = Variable("R")

  val implicits = new dsl()

  val rules: Set[Term] = Set(
    T(k(kseq(Rewrite(X, I), R)), state(statesMap(varBinding(X, I), SO))),
    T(k(kseq(Rewrite(div(And(I1, isInt(I1)), And(I2, isInt(I2))), INT.div(I1, I2)), R)), S)
  ) map (t => Rewrite(lhs(t), rhs(t)))

  env.seal()

  val rewrite = Rewriter(env)(rules)
}

//object IMP {
//  class Production0(val name: String) extends Label0 with InModule
//  class Production1(val name: String) extends Label1 with InModule
//  class Production2(val name: String) extends Label2 with InModule with SimpleNode2Label
//  class Production3(val name: String) extends Label3 with InModule
//
//  object + extends Production2("+")
//  object - extends Production2("-")
//  object <= extends Production1("<=")
//  object ! extends Production1("!")
//  object && extends Production2("&&")
//
//  object Block extends Production1("{_}")
//  object Assign extends Production1("_=_;")
//  object IfThenElse extends Production3("if_then_else_")
//  object Pgm extends Production2("int _ ; _")
//
//
//  val IdsModule = new ASSOC_LIST("_,_", new Production0(".Ids").apply())
//  val ids = IdsModule.opLabel
//  val emptyIds = IdsModule.unit
//
//  val StmtsModule = new ASSOC_LIST("_;_", new Production0(".Stmts").apply())
//  val stmts = StmtsModule.op
//  val emptyStmts = StmtsModule.unit
//
//  object TCell extends Production2("<T>")
//  object kCell extends Production1("<k>")
//  object stateCell extends Production1("<state>")
//  val StateModule = new MAP(" ", "|->", new Production0(".State").apply())
//  val state = StateModule.op
//  val emptyState = StateModule.unit
//  override def unify(a: Term, b: Term): Term = ???
//}

class ImpSpec extends FreeSpec {
  "IMP" ignore {

    import IMP.env._
    import IMP.signature._

    val term = T(k('foo), state(varBinding('foo, 5)))

    println(IMP.rewrite.searchStep(term))

    //    println(TCell(kCell(KSEQ.unit), stateCell(emptyState)))
  }
}

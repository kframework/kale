package org.kframework.km

object Imp {

  import term._
  import builtin._

  // signature

  val Id = SortOf("Id")
  val ListId = SortList(Id)

  val AExp = SortOf("AExp")
  val BExp = SortOf("BExp")
  val Exp = SortOf("Exp")
  val Block = SortOf("Block")
  val Stmt = SortOf("Stmt")
  val Pgm = SortOf("Pgm")

  val AExpInt = Constructor("_:Int->AExp", (Seq(SortInt), AExp))
  val AExpId = Constructor("_:Id->AExp", (Seq(Id), AExp))
  val AExpDiv = Constructor("_/_:AExp*AExp->AExp", (Seq(AExp, AExp), AExp))
  val AExpPlus = Constructor("_+_:AExp*AExp->AExp", (Seq(AExp, AExp), AExp))

  val BExpBool = Constructor("_:Bool->BExp", (Seq(SortBool), BExp))
  val BExpLeq = Constructor("_<=_:AExp*AExp->BExp", (Seq(AExp, AExp), BExp))
  val BExpNot = Constructor("!_:BExp->BExp", (Seq(BExp), BExp))
  val BExpAnd = Constructor("_&&_:BExp*BExp->BExp", (Seq(BExp, BExp), BExp))

  val BlockEmpty = Constructor("{}:->Block", (Seq(), Block))
  val BlockStmt = Constructor("{_}:Stmt->Block", (Seq(Stmt), Block))

  val StmtBlock = Constructor("_:Block->Stmt", (Seq(Block), Stmt))
  val StmtAssign = Constructor("_=_;:Id*AExp->Stmt", (Seq(Id, AExp), Stmt))
  val StmtIf = Constructor("if(_)_else_:BExp*Block*Block->Stmt", (Seq(BExp, Block, Block), Stmt))
  val StmtWhile = Constructor("while(_)_:BExp*Block->Stmt", (Seq(BExp, Block), Stmt))
  val StmtSeq = Constructor("__:Stmt*Stmt->Stmt", (Seq(Stmt, Stmt), Stmt))

  val PgmOf = Constructor("int_;_:List{Id}*Stmt->Pgm", (Seq(ListId, Stmt), Pgm))

  val IdsCons = Constructor("_,_:Id*List{Id}->List{Id}", (Seq(Id, ListId), ListId))
  val IdsNil = Constructor(".List:->List{Id}", (Seq(), ListId))

  val KAExp = Constructor("_:AExp->K", (Seq(AExp), SortK))
  val KBExp = Constructor("_:BExp->K", (Seq(BExp), SortK))
  val KBlock = Constructor("_:Block->K", (Seq(Block), SortK))
  val KStmt = Constructor("_:Stmt->K", (Seq(Stmt), SortK))
  val KPgm = Constructor("_:Pgm->K", (Seq(Pgm), SortK))

  // configuration

  val Cell = SortOf("Cell")

  val T = Constructor("<T>:Cell*Cell->Cell", (Seq(Cell, Cell), Cell))
  val k = Constructor("<k>:List{K}->Cell", (Seq(SortListK), Cell))
  val state = Constructor("<state>:Map{Id,Int}->Cell", (Seq(SortMapK), Cell))

  val kCons = Constructor("_~>_:K*List{K}->List{K}", (Seq(SortK, SortListK), SortListK))
  val kNil = Constructor(".K:->List{K}", (Seq(), SortListK))

  // rules

  val X = Variable("X", Id)
  val Xs = Variable("Xs", ListId)
  val M = Variable("M", SortMapK)
  val I = Variable("I", SortInt)
  val I1 = Variable("I1", SortInt)
  val I2 = Variable("I2", SortInt)
  val B = Variable("B", SortBool)
  val S = Variable("S", Stmt)
  val S1 = Variable("S1", Stmt)
  val S2 = Variable("S2", Stmt)
  val Ks = Variable("Ks", SortListK)
  val Be = Variable("Be", BExp)
  val Blk = Variable("Blk", Block)
  val Blk1 = Variable("Blk1", Block)
  val Blk2 = Variable("Blk2", Block)
  val E1 = Variable("E1", Exp)
  val E2 = Variable("E2", Exp)

  val tt = BOOL(true)

  val freezerDiv0 = Constructor("freezer_/_0:AExp->K", (Seq(AExp), SortK))
  val freezerDiv1 = Constructor("freezer_/_1:AExp->K", (Seq(AExp), SortK))

  object isKResult extends Symbol {
    override val name: String = "isKResult"
    override val smt: String = "isKResult"
    override val smtBuiltin: Boolean = false
    override val signature: Type = (Seq(SortK), SortBool)
    override val isFunctional: Boolean = true
    override def applySeq(children: Seq[Term]): Term = {
      assert(children.size == 1)
      val default = Application(this, children)
      val t = children(0)
      t match {
        case Application(`KAExp`, Seq(Application(`AExpInt`, Seq(_)))) => BOOL(true)
        case Application(`KBExp`, Seq(Application(`BExpBool`, Seq(_)))) => BOOL(true)
        case _ => BOOL(false)
      }
    }
  }

  implicit class infixTerm(p: Term) {
    def ~>:(q: Term): Term = kCons(p, q)
    def /\(q: Term): Term = BOOL.and(p, q)
  }

  val rules = Seq(
    // AExp
      SimpleRewrite(
      T(k(KAExp(AExpId(X)) ~>: Ks), state(M)),
      T(k(KAExp(AExpInt(MAP_K.select(M,X))) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KAExp(AExpDiv(AExpInt(I1), AExpInt(I2))) ~>: Ks), state(M)),
      T(k(KAExp(AExpInt(INT.div(I1, I2))) ~>: Ks), state(M)),
      BOOL.not(EQ.of(SortInt)(I1, INT(0))))
    , SimpleRewrite(
      T(k(KAExp(AExpPlus(AExpInt(I1), AExpInt(I2))) ~>: Ks), state(M)),
      T(k(KAExp(AExpInt(INT.plus(I1, I2))) ~>: Ks), state(M)),
      tt)
    // BExp
    , SimpleRewrite(
      T(k(KBExp(BExpLeq(AExpInt(I1), AExpInt(I2))) ~>: Ks), state(M)),
      T(k(KBExp(BExpBool(INT.le(I1, I2))) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KBExp(BExpNot(BExpBool(B))) ~>: Ks), state(M)),
      T(k(KBExp(BExpBool(BOOL.not(B))) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KBExp(BExpAnd(BExpBool(BOOL(true)), BExpBool(B))) ~>: Ks), state(M)),
      T(k(KBExp(BExpBool(B)) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KBExp(BExpAnd(BExpBool(BOOL(false)), BExpBool(B))) ~>: Ks), state(M)),
      T(k(KBExp(BExpBool(BOOL(false))) ~>: Ks), state(M)),
      tt)
    // Block
    , SimpleRewrite(
      T(k(KBlock(BlockEmpty()) ~>: Ks), state(M)),
      T(k(Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KBlock(BlockStmt(S)) ~>: Ks), state(M)),
      T(k(KStmt(S) ~>: Ks), state(M)),
      tt)
    // Stmt
    , SimpleRewrite(
      T(k(KStmt(StmtAssign(X, AExpInt(I))) ~>: Ks), state(M)),
      T(k(Ks), state(MAP_K.store(M,X,I))),
      tt)
    , SimpleRewrite(
      T(k(KStmt(StmtSeq(S1,S2)) ~>: Ks), state(M)),
      T(k(KStmt(S1) ~>: KStmt(S2) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KStmt(StmtIf(BExpBool(BOOL(true)), Blk1, Blk2)) ~>: Ks), state(M)),
      T(k(KStmt(Blk1) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KStmt(StmtIf(BExpBool(BOOL(false)), Blk1, Blk2)) ~>: Ks), state(M)),
      T(k(KStmt(Blk2) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KStmt(StmtWhile(Be, Blk)) ~>: Ks), state(M)),
      T(k(KStmt(StmtIf(Be, BlockStmt(StmtSeq(StmtBlock(Blk), StmtWhile(Be, Blk))), BlockEmpty())) ~>: Ks), state(M)),
      tt)
    // Pgm
    , SimpleRewrite(
      T(k(KPgm(PgmOf(IdsCons(X, Xs), S)) ~>: Ks), state(M)),
      T(k(KPgm(PgmOf(Xs, S)) ~>: Ks), state(MAP_K.store(M, X, INT(0)))),
      tt)
    , SimpleRewrite(
      T(k(KPgm(PgmOf(IdsNil(), S)) ~>: Ks), state(M)),
      T(k(KStmt(S) ~>: Ks), state(M)),
      tt)
    // strict
    , SimpleRewrite(
      T(k(KAExp(AExpDiv(E1, E2)) ~>: Ks), state(M)),
      T(k(KAExp(E1) ~>: freezerDiv0(E2) ~>: Ks), state(M)),
      BOOL.not(isKResult(KAExp(E1))))
    , SimpleRewrite(
      T(k(KAExp(AExpDiv(AExpInt(I1), E2)) ~>: Ks), state(M)),
      T(k(KAExp(E2) ~>: freezerDiv1(AExpInt(I1)) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KAExp(AExpInt(I1)) ~>: freezerDiv0(E2) ~>: Ks), state(M)),
      T(k(KAExp(AExpDiv(AExpInt(I1), E2)) ~>: Ks), state(M)),
      tt)
    , SimpleRewrite(
      T(k(KAExp(AExpInt(I2)) ~>: freezerDiv1(E1) ~>: Ks), state(M)),
      T(k(KAExp(AExpDiv(E1, AExpInt(I2))) ~>: Ks), state(M)),
      tt)


  )

}

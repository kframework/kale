package org.kframework.km

import scala.sys.process._

object z3 {

  import term._
  import builtin._

  // TODO: set proper z3 path
  private val z3 = "/Users/daejunpark/work/z3/z3-4.5.0-x64-osx-10.11.6/bin/z3"

  val cmd = Seq(z3, "-smt2", "-in")

  def run(query: String): (Int, String, String) = {
    val stdinJob: (java.io.OutputStream) => Unit = out => {
      out.write(query.getBytes())
      out.close()
    }
    var stdout: String = ""
    val stdoutJob: (java.io.InputStream) => Unit = in => {
      stdout = scala.io.Source.fromInputStream(in).getLines.mkString("\n")
      in.close()
    }
    var stderr: String = ""
    val stderrJob: (java.io.InputStream) => Unit = in => {
      stderr = scala.io.Source.fromInputStream(in).getLines.mkString("\n")
      in.close()
    }
    val pio = new ProcessIO(stdinJob, stdoutJob, stderrJob)
    val exitValue = Process(cmd).run(pio).exitValue()
    (exitValue, stdout, stderr)
  }

  case class Fail(msg: String) extends Exception

  @throws(classOf[Fail])
  def sat(term: Term): Boolean = {
    val query = declare(term) + "\n(assert " + encode(term) + ")\n" + "(check-sat)\n"
    val (exitValue, stdout, stderr) = run(query)
    if (exitValue == 0) stdout == "sat"
    else throw Fail(stdout + stderr)
  }

  @throws(classOf[Fail])
  def implies(t1: Term, t2: Term): Boolean = {
    // t1 -> t2 valid  iff  t1 /\ !t2 unsat
    !sat(BOOL.and(t1, BOOL.not(t2)))
  }

  def encode(term: Term): String = term match {
    case Application(symbol, Seq()) => symbol.smt
    case Application(symbol, children) => "(" + symbol.smt + " " + children.map(encode).mkString(" ") + ")"
    case Variable(name, _) => name
    case c:Constant => c.smt
  }

  def declare(term: Term): String = {
    trait Decl; case class Ctr(t: Symbol) extends Decl; case class Fun(t: Term) extends Decl
    // gather symbols and variables, where variables are supposed to be encoded as constants
    def getDecls(term: Term): Set[Decl] = term match {
      case Application(symbol, children) =>
        val decls = children.flatMap(getDecls).toSet
        if (symbol.smtBuiltin) decls else decls + Ctr(symbol)
      case _:Variable => Set(Fun(term))
      case _:Constant => Set()
    }
    val decls = getDecls(term)
    val (ctrs,funs) = decls.partition(_.isInstanceOf[Ctr])
    // constructor symbols group by the image sorts, as `datatypes`
    val declCtrs: String =
      ctrs.groupBy({case Ctr(t) => t.signature._2})
        .map({case (sort, ctrs) =>
            "(declare-datatypes () ((" + sort.smt + "\n" +
              ctrs.map({case Ctr(sym) =>
                "(" + sym.smt + sym.signature._1.zipWithIndex.map({case (s,i) => "(" + sym.smt + i + " " + s.smt + ")"}).mkString(" ") + ")\n"
              }).mkString +
            ")))\n"
        }).mkString
    // function (i.e., non-constructor) symbols
    // - variables and zero-argument symbols as `const`
    // - non-zero-argument symbols as `fun`
    val declFuns: String = funs.map({
        case Fun(Variable(name, sort)) => "(declare-const " + name + " " + sort.smt + ")\n"
        case Fun(sym:Symbol) if sym.signature._1 == Seq() => "(declare-const " + sym.smt + " " + sym.signature._2.smt + ")\n"
        case Fun(sym:Symbol) => "(declare-fun " + sym.smt + " (" + sym.signature._1.map(_.smt).mkString(" ") + ") " + sym.signature._2.smt + ")\n"
      }).mkString
    // remaining sorts not defined by constructor datatypes
    val sorts = decls.flatMap({
      case Ctr(symbol) => symbol.signature._1.toSet + symbol.signature._2
      case Fun(symbol:Symbol) => symbol.signature._1.toSet + symbol.signature._2
      case Fun(Variable(_, sort)) => Set(sort)
    }) -- decls.flatMap({
      case Ctr(symbol) => Set(symbol.signature._2)
      case Fun(_) => Set[Sort]()
    })
    val declSorts = sorts.map(sort => if (sort.smtBuiltin) "" else "(declare-sort " + sort.smt + ")\n").mkString
    declSorts + declFuns + declCtrs
  }

}

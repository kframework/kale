package org.kframework.km

import scala.sys.process._

class z3(val symbolsSeq: Seq[Seq[term.Symbol]]) {

  import builtin._
  import term._

  @throws(classOf[z3.Fail])
  def sat(term: Term): Boolean = term match {
    case BOOL(true) => true
    case BOOL(false) => false
    case _ => satZ3(term)
  }

  @throws(classOf[z3.Fail])
  def satZ3(term: Term): Boolean = {
    val query = declare(term) + "\n(assert " + encode(term) + ")\n" + "(check-sat)\n"
    val (exitValue, stdout, stderr) = z3.run(query)
    if (exitValue == 0) stdout == "sat"
    else throw z3.Fail(stdout + stderr)
  }

  @throws(classOf[z3.Fail])
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
    // gather functional symbols and variables, where variables are supposed to be encoded as constants
    def getFunctionSymbols(term: Term): Set[Any] = term match {
      case Application(symbol, children) =>
        val decls = children.flatMap(getFunctionSymbols).toSet
        if (!symbol.smtBuiltin && symbol.isFunctional) decls + symbol
        else decls
      case _:Variable => Set(term)
      case _:Constant => Set()
    }
    val symbols = getFunctionSymbols(term)
    // function (i.e., non-constructor) symbols
    // - variables and zero-argument symbols as `const`
    // - non-zero-argument symbols as `fun`
    val declareFuns: String = symbols.map({
        case Variable(name, sort) => "(declare-const " + name + " " + sort.smt + ")\n"
        case sym:Symbol if sym.signature._1 == Seq() => "(declare-const " + sym.smt + " " + sym.signature._2.smt + ")\n"
        case sym:Symbol => "(declare-fun " + sym.smt + " (" + sym.signature._1.map(_.smt).mkString(" ") + ") " + sym.signature._2.smt + ")\n"
      }).mkString
    // remaining sorts not defined by constructor datatypes
    val sorts = symbols.flatMap({
      case symbol:Symbol => symbol.signature._1.toSet + symbol.signature._2
      case Variable(_, sort) => Set(sort)
    })
    val declareSorts = (sorts -- datatypes)
      .map(sort => if (sort.smtBuiltin) "" else "(declare-sort " + sort.smt + ")\n").mkString
    declareSorts + declDatatypes + declareFuns
  }
  lazy val datatypes: Set[Sort] = symbolsSeq.flatMap(_.flatMap(s => s.signature._1.toSet + s.signature._2).toSet).toSet
  lazy val declDatatypes: String = declareDatatypesSeq(symbolsSeq)

  // symbolsSeq: SCCs of symbols in topological order
  def declareDatatypesSeq(symbolsSeq: Seq[Seq[Symbol]]): String = symbolsSeq.map(declareDatatypes).mkString
  def declareDatatypes(symbols: Seq[Symbol]): String = {
    "(declare-datatypes () (\n" +
      symbols.filter(sym => !sym.isFunctional && !sym.smtBuiltin)
        .groupBy(_.signature._2)
        .map({case (sort, syms) =>
          "  (" + sort.smt + "\n" +
            syms.map(sym =>
              "    (" + sym.smt + " " + sym.signature._1.zipWithIndex.map({case (s,i) => "(" + sym.smt + i + " " + s.smt + ")"}).mkString(" ") + ")\n"
            ).mkString +
            "  )\n"
        }).mkString +
    "))\n"
  }

}

object z3 {

  // TODO: set proper z3 path
  private val z3 = "/home/virgil/runtime-verification/z3/build/z3" // "/usr/bin/z3" // "./z3/bin/z3" // "z3-4.5.0-x64-osx-10.11.6/bin/z3"

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

}

package org.kframework.kale

import scala.collection._
import scala.sys.process._

trait Z3Builtin

class z3(val env: Environment) {

  import env._

  @throws(classOf[z3.Fail])
  def sat(term: Term): Boolean = term match {
    case Top => true
    case Bottom => false
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
    !sat(And(t1, Not(t2)))
  }

  def encode(term: Term): String = term match {
    case t:Equals => "(= " + encode(t._1) + " " + encode(t._2) + ")"
    case t:And => "(and " + encode(t._1) + " " + encode(t._2) + ")"
    case t:Or => "(or " + encode(t._1) + " " + encode(t._2) + ")"
    case FreeNode0(symbol) => symbol.smt
    case t:FreeNode => "(" + t.label.smt + " " + t.children.map(encode).mkString(" ") + ")"
    case v:Variable => v.name.toString
    case c:DomainValue[_] => c.toString
    case _ => ???
  }

  def declare(term: Term): String = {
    // gather functional symbols and variables, where variables are supposed to be encoded as constants
    def getFunctionSymbols(term: Term): Set[Any] = term match {
      case t:Node =>
        val decls = t.children.flatMap(getFunctionSymbols).toSet
        if (!t.label.isInstanceOf[Z3Builtin]) decls + t.label
        else decls
      case _:Variable => Set(term)
      case _:DomainValue[_] => Set()
      case _ => ???
    }
    val symbols = getFunctionSymbols(term)
    // function (i.e., non-constructor) symbols
    // - variables and zero-argument symbols as `const`
    // - non-zero-argument symbols as `fun`
    val declareFuns: String = symbols.map({
      case v:Variable => "(declare-const " + v.name + " " + v.sort.name + ")\n"
      case l:FreeLabel0 => "(declare-const " + l.smt + " " + sortTarget(l).name + ")\n"
      case l:FreeLabel => "(declare-fun " + l.smt + " (" + sortArgs(l).map(_.name).mkString(" ") + ") " + sortTarget(l).name + ")\n"
      case _ => ???
    }).mkString
    // remaining sorts not defined by constructor datatypes
    val sorts: Set[Sort] = symbols.flatMap({
      case l:FreeLabel => sortArgs(l).toSet + sortTarget(l)
      case v:Variable => Set(v.sort)
      case _ => ???
    })
    val declareSorts = (sorts -- datatypes)
      .map(sort => if (sort.isInstanceOf[Z3Builtin]) "" else "(declare-sort " + sort.name + ")\n").mkString
    declareSorts + declDatatypes + declareFuns
  }
  lazy val datatypes: Set[Sort] = Set()
  lazy val declDatatypes: String = ""

  // TODO:
//  lazy val datatypes: Set[Sort] = symbolsSeq.flatMap(_.flatMap(s => s.signature._1.toSet + s.signature._2).toSet).toSet
//  lazy val declDatatypes: String = declareDatatypesSeq(symbolsSeq)
//
//  // symbolsSeq: SCCs of symbols in topological order
//  def declareDatatypesSeq(symbolsSeq: Seq[Seq[Symbol]]): String = symbolsSeq.map(declareDatatypes).mkString
//  def declareDatatypes(symbols: Seq[Symbol]): String = {
//    "(declare-datatypes () (\n" +
//      symbols.filter(sym => !sym.isFunctional && !sym.smtBuiltin)
//        .groupBy(_.signature._2)
//        .map({case (sort, syms) =>
//          "  (" + sort.smt + "\n" +
//            syms.map(sym =>
//              "    (" + sym.smt + " " + sym.signature._1.zipWithIndex.map({case (s,i) => "(" + sym.smt + i + " " + s.smt + ")"}).mkString(" ") + ")\n"
//            ).mkString +
//            "  )\n"
//        }).mkString +
//      "))\n"
//  }

}

object z3 {

  // TODO: set proper z3 path
  private val z3 = "../z3/bin/z3" // "z3-4.5.0-x64-osx-10.11.6/bin/z3"

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


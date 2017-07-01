package org.kframework.kale

import org.kframework.kale.km.{MultisortedMixing, Z3Mixin}

import scala.collection._
import scala.sys.process._

trait Z3Builtin

/*
  symbolsSeq: constructor symbols that need to be encoded using z3 datatypes instead of functions.
  It should be given as SCCs of symbols in topological order of dependency.
 */
class z3(val env: Environment with MultisortedMixing with Z3Mixin, val symbolsSeq: Seq[Seq[Label]]) {

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
    case FreeNode0(symbol) => symbol.smtName
    case t:FreeNode => "(" + t.label.smtName + " " + t.children.map(encode).mkString(" ") + ")"
    case v:Variable => v.name.toString
    case c:DomainValue[_] => c.toString
    case _ => ???
  }

  def declare(term: Term): String = {
    // gather functional symbols and variables, where variables are supposed to be encoded as constants
    def getFunctionSymbols(term: Term): Set[Any] = term match {
      case t:Node =>
        val decls = t.children.flatMap(getFunctionSymbols).toSet
        if (!env.isZ3Builtin(t.label)) decls + t.label
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
      case v:Variable => "(declare-const " + v.name + " " + v.sort.smtName + ")\n"
      case l:FreeLabel0 => "(declare-const " + l.smtName + " " + sort(l).smtName + ")\n"
      case l:FreeLabel => "(declare-fun " + l.smtName + " (" + sortArgs(l).map(_.smtName).mkString(" ") + ") " + sort(l).smtName + ")\n"
    }).mkString
    // remaining sorts not defined by constructor datatypes
    val sorts: Set[Sort] = symbols.flatMap({
      case l:FreeLabel => sortArgs(l).toSet + sort(l)
      case v:Variable => Set(v.sort)
      case _ => ???
    })
    val declareSorts = (sorts -- datatypes)
      .map(sort => if (sort.isInstanceOf[Z3Builtin]) "" else "(declare-sort " + sort.smtName + ")\n").mkString
    declareSorts + declDatatypes + declareFuns
  }
//lazy val datatypes: Set[Sort] = symbolsSeq.flatMap(_.flatMap(s => sortArgs(s).toSet + sort(s)).toSet).toSet
  lazy val datatypes: Set[Sort] = symbolsSeq.flatMap(_.map(sort).toSet).toSet
  lazy val declDatatypes: String = declareDatatypesSeq(symbolsSeq)

  // symbolsSeq: SCCs of symbols in topological order
  def declareDatatypesSeq(symbolsSeq: Seq[Seq[Label]]): String = symbolsSeq.map(declareDatatypes).mkString
  def declareDatatypes(symbols: Seq[Label]): String = {
    "(declare-datatypes () (\n" +
      symbols.filter(sym => !sym.isInstanceOf[Z3Builtin])
        .groupBy(sort)
        .map({case (sort, syms) =>
          "  (" + sort.smtName + "\n" +
            syms.map(sym =>
              "    (" + sym.smtName + " " + sortArgs(sym).zipWithIndex.map({case (s,i) => "(" + sym.smtName + i + " " + s.smtName + ")"}).mkString(" ") + ")\n"
            ).mkString +
            "  )\n"
        }).mkString +
      "))\n"
  }

}

object z3 {

  // TODO: set proper z3 path
  private val z3 = "./z3/bin/z3" // "z3-4.5.0-x64-osx-10.11.6/bin/z3"

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


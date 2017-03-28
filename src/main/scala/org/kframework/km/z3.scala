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
    val query = declare(term) + "(assert " + encode(term) + ")" + "(check-sat)"
    val (exitValue, stdout, stderr) = run(query)
    if (exitValue == 0) stdout == "sat"
    else throw Fail(stdout + stderr)
  }

  def encode(term: Term): String = term match {
    case Application(symbol, Seq()) => symbol.smt
    case Application(symbol, children) => "(" + symbol.smt + " " + children.map(encode).mkString(" ") + ")"
    case Variable(name, _) => name
    case c:Constant => c.smt
  }

  def declare(term: Term): String = {
    def gatherTypes(types: Map[String, Type], term: Term): Map[String, Type] = term match {
      case Application(symbol, children) =>
        val _types = if (symbol.smtBuiltin) types else types + (symbol.name -> symbol.signature)
        children.foldLeft(_types)(gatherTypes)
      case Variable(name, sort) => types + (name -> (Seq(), sort))
      case _:Constant => types
    }
    val symbols: Map[String, Type] = gatherTypes(Map(), term)
    val declareSymbols = symbols.map({ case (l,typ) =>
        typ match {
          case (Seq(), s:Sort) => "(declare-const " + l + " " + s.smt + ")"
          case (ss:Seq[Sort], s:Sort) => "(declare-fun " + l + " (" + ss.map(_.smt).mkString(" ") + ") " + s.smt + ")"
        }
    }).mkString("\n")
    val sorts = symbols.values.foldLeft(Set[Sort]())((sorts,typ) => sorts ++ typ._1.toSet + typ._2)
    val declareSorts = sorts.map(sort => if (sort.smtBuiltin) "" else "(declare-sort " + sort.smt + ")").mkString("\n")
    declareSorts + declareSymbols
  }
}

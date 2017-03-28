package org.kframework.km

import scala.sys.process._

object z3 {
  // TODO: set proper z3 path
  private val z3 = "/Users/daejunpark/work/z3/z3-4.5.0-x64-osx-10.11.6/bin/z3"

  val cmd = Seq(z3, "-smt2", "-in")

  case class Fail(msg: String) extends Exception

  @throws(classOf[Fail])
  def sat(query: String): Boolean = {
    var stdout: String = ""
    var stderr: String = ""

    val stdinJob: (java.io.OutputStream) => Unit = out => {
      out.write(query.getBytes())
      out.close()
    }
    val stdoutJob: (java.io.InputStream) => Unit = in => {
      stdout = scala.io.Source.fromInputStream(in).getLines.mkString("\n")
      in.close()
    }
    val stderrJob: (java.io.InputStream) => Unit = in => {
      stderr = scala.io.Source.fromInputStream(in).getLines.mkString("\n")
      in.close()
    }
    val pio = new ProcessIO(stdinJob, stdoutJob, stderrJob)
    val exitValue = Process(cmd).run(pio).exitValue()

    if (exitValue == 0) stdout == "sat"
    else throw Fail(stdout + stderr)
  }
}

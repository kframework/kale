package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.builtin.importINT
import org.kframework.kale.standard.DNFEnvironment
import org.scalatest.FreeSpec

class FooTest extends FreeSpec{
  "foo" in {
    implicit val env = new DNFEnvironment() {
      override lazy val substitutionMaker: (Substitution) => SubstitutionApply = ???
      override protected lazy val unifier: MatcherOrUnifier = ???

      override def sort(l: Label, children: Seq[Term]): Sort = ???

      override def sortArgs(l: Label): Seq[Sort] = ???

      override def sortTarget(l: Label): Sort = ???

      override def SMTName(l: Label): String = ???

      override def isZ3Builtin(l: Label): Boolean = ???
    }

    println(env.isInstanceOf[importINT])

  }
}

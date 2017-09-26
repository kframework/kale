package org.kframework.kale.tests

import org.kframework.kale.Term
import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.util.Path
import org.scalatest.FreeSpec

class PathTest extends TestSetup[StandardEnvironment]() {

  import env._

  "apply empty path" in {
    assert(Path(Seq())(foo(bar(1), 2)) === foo(bar(1), 2))
  }

  "apply path one way" in {
    assert(Path(Seq(1))(foo(bar(1), 2)) === (2: Term))
  }

  "apply path another way" in {
    assert(Path(Seq(0, 0))(foo(bar(1), 2)) === (1: Term))
  }

  "apply to third element in assoc" in {
    assert(Path(Seq(2))(el ~~ 1 ~~ 2 ~~ 3) === (3: Term))
  }

  "explicitate" in {
    assert(Path(Seq(0, 0, 1)).explicitate(foo(bar(el ~~ 1 ~~ 2), 2)) === Seq(foo, bar, listLabel, INT.Int))
  }
}

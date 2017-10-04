package org.kframework.kale.tests

import org.kframework.kale.Term
import org.kframework.kale.standard.StandardEnvironment

class PathTest extends TestSetup[StandardEnvironment]() {

  import env._

  "apply empty path" in {
    assert(Path(List())(foo(bar(1), 2)) === foo(bar(1), 2))
  }

  "apply path one way" in {
    assert(Path(List(1))(foo(bar(1), 2)) === (2: Term))
  }

  "apply path another way" in {
    assert(Path(List(0, 0))(foo(bar(1), 2)) === (1: Term))
  }

  "apply to third element in assoc" in {
    assert(Path(List(2))(el ~~ 1 ~~ 2 ~~ 3) === (3: Term))
  }

  "explicitate" in {
    assert(Path(List(0, 0, 1)).explicitate(foo(bar(el ~~ 1 ~~ 2), 2)) === List(foo, bar, listLabel, INT.Int))
  }
}

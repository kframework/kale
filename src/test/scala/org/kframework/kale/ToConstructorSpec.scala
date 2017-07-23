package org.kframework.kale

import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.tests.TestSetup

class ToConstructorSpec extends TestSetup[StandardEnvironment]() {
  import env._

  "simple node" in {
    assert(bar("1").toConstructor == "bar(String(\"\"\"1\"\"\"))")
  }
}

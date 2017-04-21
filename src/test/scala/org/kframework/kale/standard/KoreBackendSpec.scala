package org.kframework.kale.standard

import org.scalatest.FreeSpec

class KoreBackendSpec extends FreeSpec {
  import org.kframework.kore.implementation.DefaultBuilders._
  "empty definition" in {
    val d = Definition(Seq(Module(ModuleName("A"), Seq(), Attributes(Seq()))), Attributes(Seq()))

    new KoreBackend(d, ModuleName("A"))
  }
}

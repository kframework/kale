package org.kframework.kale.kore

import org.kframework.kale.Environment
import org.kframework.kore.{Pattern, Rewriter}

trait KaleRewriter extends Rewriter {
  implicit val env: Environment

  override def step(t: Pattern): Pattern = ???
}

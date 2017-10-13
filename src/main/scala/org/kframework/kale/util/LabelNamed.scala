package org.kframework.kale.util

import org.kframework.kale.Environment

abstract class LabelNamed[E <: Environment](val name: String)(implicit val env: E)

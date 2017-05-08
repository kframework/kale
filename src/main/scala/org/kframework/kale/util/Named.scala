package org.kframework.kale.util

import org.kframework.kale.{Environment, Label}

abstract class Named[E <: Environment](val name: String)(implicit val env: E)

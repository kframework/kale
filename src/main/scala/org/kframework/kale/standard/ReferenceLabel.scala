package org.kframework.kale.standard

import org.kframework.kale._



abstract class ReferenceLabel[T](val name: String)(val env: Environment) extends PrimordialConstantLabel[T]

package org.kframework.kale

import org.kframework.kale.standard.ScalaLibraryMixin

trait Up[O] extends (O => Term)

class IntUp(implicit env: Environment with IntMixin) extends Up[Int] {
  override def apply(i: Int) = env.INT.Int(i)
}

class ScalaListUp[O: Up](implicit env: Environment with ScalaLibraryMixin) extends Up[List[O]] {
  override def apply(l: List[O]) = env.scalaList(l map implicitly[Up[O]])
}

object Up {
  final def apply[O](implicit instance: Up[O]): Up[O] = instance

  final def apply[O](f: O => Term): Up[O] = new Up[O] {
    override def apply(o: O): Term = f(o)
  }
}

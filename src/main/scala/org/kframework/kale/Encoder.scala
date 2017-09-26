package org.kframework.kale

trait Encoder[O] extends (O => Term)

class IntEncoder(implicit env: Environment with IntMixin) extends Encoder[Int] {
  override def apply(i: Int) = env.INT.Int(i)
}

class ScalaListEncoder[O: Encoder](implicit env: Environment with ScalaTermsMixin) extends Encoder[List[O]] {
  override def apply(l: List[O]) = env.scalaList(l map implicitly[Encoder[O]])
}

object Encoder {
  final def apply[O](implicit instance: Encoder[O]): Encoder[O] = instance

  final def apply[O](f: O => Term): Encoder[O] = new Encoder[O] {
    override def apply(o: O): Term = f(o)
  }
}

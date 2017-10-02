package org.kframework.kale

import cats.Foldable
import org.kframework.kale.standard.ScalaLibraryMixin

trait Up[O] extends (O => Term)

trait MonoidLabeled[O[_]] {
  def monoidLabel: MonoidLabel
}

class ScalaListUp[O: Up](implicit env: Environment with ScalaLibraryMixin) extends Up[List[O]] {
  override def apply(l: List[O]) = env.scalaList(l map implicitly[Up[O]])
}

object Up {
  final def apply[O](implicit instance: Up[O]): Up[O] = instance

  final def apply[O](f: O => Term): Up[O] = new Up[O] {
    override def apply(o: O): Term = f(o)
  }

  implicit def upMonoidLabeled[O[_] : MonoidLabeled : Foldable, E: Up]: Up[O[E]] = new Up[O[E]] {
    override def apply(o: O[E]) = {
      implicitly[Foldable[O]].foldMap(o)(implicitly[Up[E]])(implicitly[MonoidLabeled[O]].monoidLabel)
    }
  }
}

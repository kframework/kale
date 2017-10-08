package org.kframework.kale.standard

import org.kframework.kale.builtin.IntMixin
import org.kframework.kale.util.Named
import org.kframework.kale.{SemigroupLabel, DefineMixin, Environment, FreeLabel1, Label, Mixin, Term, UpDown}

trait PathMixin extends Mixin {
  _: Environment with DefineMixin with IntMixin with ScalaLibraryMixin =>


  val PathLabel = new Named("PathLabel") with FreeLabel1

  implicit val updownPath = new UpDown[Path] {
    val updownList = implicitly[UpDown[List[Int]]]

    override def unapply(t: Term) =
      updownList.unapply(t.children.head) map Path

    override def apply(o: Path) =
      PathLabel(updownList(o.positions))
  }

  case class map0WithPath(f: (Term, Path) => Term) {
    def apply(t: Term, path: Path): Term = t.copy(t.flattenedChildren.zipWithIndex map {
      case (t: Term, i: Int) => f(t, Path(path.positions :+ i))
    })
  }

  /**
    * works only for non-comm
    */
  case class Path(positions: List[Int]) {
    /**
      * Returns the subterm of t at the current Path position
      */
    def apply(t: Term): Term = positions match {
      case head :: tail =>
        val elements = t.flattenedChildren
        Path(tail)(elements(positions.head))
      case Nil => t
    }

    /**
      * Displays the sequence of instructions encountered when traversing term t with this path.
      */
    def explicitate(t: Term): Seq[Label] = positions match {
      case head :: tail =>
        val elements = t.label match {
          case label: SemigroupLabel => label.asIterable(t).toSeq
          case _ => t.children.toSeq
        }
        t.label +: Path(tail).explicitate(elements(positions.head))
      case Nil => Seq(t.label)
    }
  }


}
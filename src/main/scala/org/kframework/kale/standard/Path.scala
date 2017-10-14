package org.kframework.kale.standard

import org.kframework.kale.builtin.IntMixin
import org.kframework.kale.util.LabelNamed
import org.kframework.kale.{SemigroupLabel, LiftedCatsMixin, Environment, FreeLabel1, Label, Mixin, Term, UpDown}

trait PathMixin extends Mixin {
  _: Environment with LiftedCatsMixin with IntMixin with ScalaLibraryMixin =>


  implicit val PathLabel = free1(Path)

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
package org.kframework.kale.standard

import org.kframework.kale.util.Named
import org.kframework.kale.{AssocLabel, Environment, FreeLabel1, Label, Mixin, Term, UpDown}

trait PathMixin extends Mixin {
  _: Environment =>


  val PathLabel = new Named("PathLabel") with FreeLabel1

  /**
    * works only for non-comm
    */
  case class Path(positions: List[Int]) {
    /**
      * Returns the subterm of t at the current Path position
      */
    def apply(t: Term): Term = positions match {
      case head :: tail =>
        val elements = t.label match {
          case label: AssocLabel => label.asIterable(t).toSeq
          case _ => t.children.toSeq
        }
        Path(tail)(elements(positions.head))
      case Nil => t
    }

    /**
      * Displays the sequence of instructions encountered when traversing term t with this path.
      */
    def explicitate(t: Term): Seq[Label] = positions match {
      case head :: tail =>
        val elements = t.label match {
          case label: AssocLabel => label.asIterable(t).toSeq
          case _ => t.children.toSeq
        }
        t.label +: Path(tail).explicitate(elements(positions.head))
      case Nil => Seq(t.label)
    }
  }


}
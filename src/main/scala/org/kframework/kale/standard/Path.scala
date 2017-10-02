package org.kframework.kale.standard

import org.kframework.kale.{AssocLabel, Label, Term}

/**
  * works only for non-comm
  */
case class Path(positions: Seq[Int]) {
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
